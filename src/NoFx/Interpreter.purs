module Interpreter where

import Prelude (class Eq, class Ord, (+), (-), (*), (/), (<>), (==), (/=), (<), (<=), (>), (>=), (<<<), (>>>), ($), (<$>), negate, show, otherwise)

import Data.List (List(..), (:), (!!), drop, head, last, length, null, singleton, tail, take)
import Data.Maybe (Maybe(..))
import Data.Tuple (lookup)
import Data.Tuple.Nested

import Partial.Unsafe(unsafeCrashWith, unsafePartial)

import AST
import Compiler
import Utility

interpret :: GmState -> List GmState
interpret s = s:ss
  where ss | null s.code = Nil
           | otherwise = interpret (countSteps (step s))

countSteps :: GmState -> GmState
countSteps s = s { stats = s.stats + 1 }

step :: GmState -> GmState
step s = go s.code
  where
    go (i:is) = dispatch i (s { code = is })
    go Nil = unsafeCrashWith "No code to execute"

isUnwind :: GmState -> GmState
isUnwind s = s { isUnwind = true }

isNotUnwind :: GmState -> GmState
isNotUnwind s = s { isUnwind = false }

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f)  = isNotUnwind >>> pushglobal f
dispatch (PushInt n)     = isNotUnwind >>> pushint n
dispatch (Push n)        = isNotUnwind >>> push n
dispatch (Slide n)       = isNotUnwind >>> slide n
dispatch (Mkap)          = isNotUnwind >>> mkap
dispatch (Pop n)         = isNotUnwind >>> pop n
dispatch (Alloc n)       = isNotUnwind >>> alloc n
dispatch (Update n)      = isNotUnwind >>> update n
dispatch (Eval)          = isNotUnwind >>> eval
dispatch (Pack n t a)    = isNotUnwind >>> pack n t a
dispatch (Casejump alts) = isNotUnwind >>> casejump alts
dispatch (Unwind)        = isUnwind    >>> unwind
dispatch (Print)         = isNotUnwind >>> print
dispatch (Split n)       = isNotUnwind >>> split n
dispatch (Add)           = isNotUnwind >>> arithmetic2 (+)
dispatch (Sub)           = isNotUnwind >>> arithmetic2 (-)
dispatch (Mul)           = isNotUnwind >>> arithmetic2 (*)
dispatch (Div)           = isNotUnwind >>> arithmetic2 (/)
dispatch (Neg)           = isNotUnwind >>> arithmetic1 (negate)
dispatch (Eq)            = isNotUnwind >>> comparison (==)
dispatch (Neq)           = isNotUnwind >>> comparison (/=)
dispatch (Lt)            = isNotUnwind >>> comparison (<)
dispatch (Leq)           = isNotUnwind >>> comparison (<=)
dispatch (Gt)            = isNotUnwind >>> comparison (>)
dispatch (Geq)           = isNotUnwind >>> comparison (>=)
dispatch x               = unsafeCrashWith $ "No such instruction " <> show x

pushglobal :: Name -> GmState -> GmState
pushglobal f s =
  let a = case lookup f (s.globals) of
                    Just g -> g
                    Nothing -> unsafeCrashWith $ "Undeclared global " <> f
  in s { stack = a:s.stack }

mkap :: GmState -> GmState
mkap = unsafePartial $ mkap'
  where
    mkap' :: Partial => GmState -> GmState
    mkap' s = let (a1:a2:as) = s.stack
                  (heap' /\ a) = hAlloc (s.heap) (NAp a1 a2)
              in s { heap = heap', stack = a:as }

casejump :: List (Int /\ GmCode) -> GmState -> GmState
casejump alts s = s { code = code' }
  where
    addr = case head s.stack of
                 Just h -> h
                 Nothing -> unsafeCrashWith "Empty stack"
    tag = case hLookup s.heap addr of
                NConstr _ tag _ -> tag
                x -> unsafeCrashWith $ "Expected a constructor" <> show x
    selected = case lookup tag alts of
                    Just s -> s
                    Nothing -> unsafeCrashWith $ "Casejump failed to find branch " <> show tag
    code' = selected <> s.code

push :: Int -> GmState -> GmState
push n s = s { stack = val : s.stack }
  where val = case s.stack !! n of
                Just s -> s
                Nothing -> unsafeCrashWith "Stack does not have enough arguments"

pushint :: Int -> GmState -> GmState
pushint n s = case lookup (show n) s.globals of
                    Nothing -> let (heap' /\ addr) = hAlloc s.heap $ NNum n
                                   stack' = (addr : s.stack)
                                   globals' = ((show n) /\ addr) : s.globals
                               in s { stack = stack', heap = heap', globals = globals' }
                    (Just addr) -> s { stack = addr:s.stack }

pop :: Int -> GmState -> GmState
pop n s = s { stack = drop n s.stack }

eval :: GmState -> GmState
eval s = s' { dump = dump' }
  where
    i = s.code
    (a /\ as) = case s.stack of
                  Cons a as -> (a /\ as)
                  Nil -> unsafeCrashWith "Not enough arguemnts on stack 0"
    s' = s { code = singleton Unwind, stack = singleton a }
    dump' = (i /\ as) : (s.dump)

update :: Int -> GmState -> GmState
update n s = s'
  where
    oldAddr = case s.stack !! (n + 1) of
                   Just s -> s
                   Nothing -> unsafeCrashWith ""
    ind = case (head s.stack) of
               Just h -> h
               Nothing -> unsafeCrashWith "Not enough arguments on stack 1"
    (newHeap /\ newAddr) = hAlloc s.heap $ NInd ind
    (a /\ as) = splitAt (n + 1) s.stack
    tempStack = a <> (newAddr : (drop 1 as))
    newStack = drop 1 tempStack
    s' = s { stack = newStack, heap = newHeap }

slide :: Int -> GmState -> GmState
slide n s = s { stack = (st : drop n ack) }
  where (st /\ ack) = case s.stack of
                      Cons st ack -> (st /\ ack)
                      Nil -> unsafeCrashWith ""

split :: Int -> GmState -> GmState
split n s = s { stack = as <> ack }
  where (st /\ ack) = case s.stack of
                        Cons st ack -> (st /\ ack)
                        Nil -> unsafeCrashWith "Not enough arguments on stack 2"
        as = case hLookup s.heap st of
                  NConstr _ _ as -> as
                  _ -> unsafeCrashWith "Expecting constructor"

alloc :: Int -> GmState -> GmState
alloc n s = s { heap = heap', stack = stack' <> s.stack }
  where (heap' /\ stack') = allocNodes n s.heap

pack :: String -> Int -> Int -> GmState -> GmState
pack n t a s = s { heap = heap', stack = (addr : drop a s.stack) }
  where (heap' /\ addr) = hAlloc s.heap (NConstr n t (take a s.stack))

unwind :: GmState -> GmState
unwind s =
  newState (hLookup s.heap a)
  where
    (a /\ as) = case s.stack of
                  Cons a as -> (a /\ as)
                  Nil -> unsafeCrashWith "Not enough arguments on stack 3"
    newState (NNum n)
      | s.dump == Nil = s
      | otherwise = s { dump = ds, code = c, stack = a:s' }
      where
        (c /\ s' /\ ds) = case s.dump of
                              (c /\ s'):ds -> (c /\ s' /\ ds)
                              _ -> unsafeCrashWith ""
    newState (NConstr _ n as)
      | s.dump == Nil = s
      | otherwise = s { dump = ds, code = c, stack = a:s' }
      where
        (c /\ s' /\ ds) = case s.dump of
                              (c /\ s'):ds -> (c /\ s' /\ ds)
                              _ -> unsafeCrashWith ""
    newState (NInd a1) =  s { code = singleton Unwind, stack = (a1:as) }
    newState (NAp a1 a2) = s { code = singleton Unwind, stack = (a1:a:as) }
    newState (NGlobal _ n c')
      | (length as >= n) = s { code = c', stack = rearrange n s.heap (a:as) }
      | otherwise =
        let stack' = case (last (s.stack <> s')) of
                          Just s -> s
                          Nothing -> unsafeCrashWith "Not enough arguments on stack 4"
        in s { dump = ds, code = c, stack = singleton stack' }
      where
        (c /\ s' /\ ds) = case s.dump of
                              (c /\ s'):ds -> (c /\ s' /\ ds)
                              _ -> unsafeCrashWith ""

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
getArg _ = unsafeCrashWith ""

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stack = take n stack' <> drop n stack
  where tl = case tail stack of
                  Just tl -> tl
                  Nothing -> unsafeCrashWith ""
        stack' = (getArg <<< hLookup heap) <$> tl

print :: GmState -> GmState
print s = newState (hLookup s.heap a)
  where
        (a /\ as) = case s.stack of
                          (a:as) -> (a /\ as)
                          _ -> unsafeCrashWith ""
        newState (NConstr name t arity) =
                                     s { stack = arity <> as
                                       , output = name
                                       , code = printcode (length arity) <>
                                                s.code
                                       }
        newState (NNum n) = s { output = show n, stack = as }
        newState _ = unsafeCrashWith "Non"
        printcode 0 = Nil
        printcode n = (Eval : Print : (printcode (n-1)))

binaryOp :: forall a b. Eq a => Ord a => (b -> GmState -> GmState)
           -> (Addr -> GmState -> a)
           -> (a -> a -> b)
           -> (GmState -> GmState)
binaryOp box unbox op s
  = box (op (unbox a0 s) (unbox a1 s)) s { stack = as }
  where
    (a0 /\ a1 /\ as) = case s.stack of
                      a0:a1:as -> (a0 /\ a1 /\ as)
                      _ -> unsafeCrashWith ""

unaryOp :: forall a b.(b -> GmState -> GmState)
        -> (Addr -> GmState -> a)
        -> (a -> b)
        -> (GmState -> GmState)
unaryOp box unbox op s
  = box (op (unbox a s)) (s { stack = as })
  where
    (a /\ as) = case s.stack of
                  (a:as) -> (a /\ as)
                  Nil -> unsafeCrashWith ""

boxInteger :: Int -> GmState -> GmState
boxInteger n s =
  s { stack = (a:s.stack), heap = h' }
  where
    (h' /\ a) = hAlloc s.heap (NNum n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger a s =
  ub (hLookup s.heap a)
  where
    ub (NNum i) = i
    ub (NConstr _ t _) = t
    ub x        = unsafeCrashWith $ "Unboxing non-integer" <> show x

boxBoolean :: Boolean -> GmState -> GmState
boxBoolean b s =
  s { stack = a:s.stack, heap = h' }
  where
    b' | b = 2
       | otherwise = 3
    name = if b' == 2 then "true" else "false"
    (h' /\ a) = hAlloc s.heap (NConstr name b' Nil)

arithmetic2 = binaryOp boxInteger unboxInteger

arithmetic1 = unaryOp boxInteger unboxInteger

comparison op = binaryOp boxBoolean unboxInteger op'
  where op' x y | op x y = true
                | otherwise = false
