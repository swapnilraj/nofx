module Compiler where

import Prelude(class Show, show, (<$>), (<>), ($), (-), (+), (==), flip, otherwise)

import Control.Apply

import Data.List(List(..), (:), (..), elem, length, fromFoldable, reverse, singleton, zip)
import Data.Tuple(lookup, fst, snd)
import Data.Tuple.Nested
import Data.Maybe(Maybe(..), fromJust)
import Data.Generic.Rep
import Data.Generic.Rep.Show

import Debug.Trace

import Partial.Unsafe(unsafePartial)

import Utility
import AST

type GmCode = List Instruction
type GmOutput = List Char
type Addr = Int
type GmStack = List Addr

type GmDumpItem = GmCode /\ GmStack
type GmDump = List GmDumpItem

type GmHeap = Heap Node

type GmGlobals = ASSOC Name Addr
type GmStats = Int

data GmCompiledSC = CompSC Name Int GmCode
derive instance genericGmCompiledSC :: Generic (GmCompiledSC) _
instance showGmCompiledSC :: Show (GmCompiledSC) where
  show x = genericShow x

type GmEnvironment = ASSOC Name Int
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmState
  = { output  :: GmOutput
    , code    :: GmCode
    , stack   :: GmStack
    , dump    :: GmDump
    , heap    :: GmHeap
    , globals :: GmGlobals
    , stats   :: GmStats
    }

data Instruction
  = Slide Int
  | Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | Mkap
  | Pop Int
  | Alloc Int
  | Update Int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq  | Neq
  | Lt | Leq | Gt | Geq
  | Cond GmCode GmCode
  | Pack Int Int
  | Casejump (List (Int /\ GmCode))
  | Split Int
derive instance genericInstruction :: Generic (Instruction) _
instance showInstruction :: Show (Instruction) where
  show x = genericShow x

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int (List Addr)
derive instance genericNode :: Generic (Node) _
instance showNode :: Show (Node) where
  show x = genericShow x

compile :: CoreProgram -> GmState
compile prog
  = { output: Nil
    , code: initialCode
    , stack: Nil
    , dump: Nil
    , heap: h
    , globals: g
    , stats: initialStat
    }
    where
      (h /\ g) = buildInitialHeap prog

initialStat :: GmStats
initialStat = 0

initialCode :: GmCode
initialCode = fromFoldable [PushGlobal "main", Eval]

hAlloc :: forall a. Heap a -> a -> (Heap a /\ Addr)
hAlloc = unsafePartial $ hAlloc'
  where
    hAlloc' :: Partial => forall a. Heap a -> a -> (Heap a /\ Addr)
    hAlloc' (size /\ (next:free) /\ cts) n =
        (((size+1) /\ (free /\ (next /\ n) : cts)) /\ next)

hInitial = (0 /\ (1..100) /\ Nil)

aDomain :: forall a b. ASSOC a b -> List a
aDomain alist = ado (a /\ b) <- alist in a

aRange :: forall a b. ASSOC a b -> List b
aRange alist = ado (a /\ b) <- alist in b

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap /\ (Name /\ Addr))
allocateSc heap (CompSC name nargs instns)
  = heap' /\ (name /\ addr)
  where (heap' /\ addr) = hAlloc heap (NGlobal nargs instns)

buildInitialHeap :: CoreProgram -> (GmHeap /\ GmGlobals)
buildInitialHeap prog
  = mapAccuml allocateSc hInitial $ trace (show compiled) \_ -> compiled
  where compiled = (compileSc <$> prog) <> compiledPrimitives

compileSc :: CoreSC -> GmCompiledSC
compileSc (Func name env body)
  = CompSC name len $ compileR body (zip env (0..len))
    where len = length env

compileR :: GmCompiler
compileR e args = compileE e args <> fromFoldable [Update d, Pop d, Unwind]
  where d = length args

compileE' :: Int -> GmCompiler
compileE' offset expr args
  = singleton (Split offset) <> compileE expr args <> singleton (Slide offset)

compileE :: GmCompiler
compileE (Num n) args = fromFoldable [PushInt n]
compileE (Var v) args
  | elem v (aDomain args) = singleton $
                            Push (unsafePartial $ fromJust $ lookup v args)
  | otherwise = singleton $ PushGlobal v
compileE (Constr n t 0) _ = fromFoldable [Pack t 0]
compileE inst@(App (App (Var op) e1) e2) args
  = case lookup op builtInDyadic of
      Just op' -> compileE e2 args <>
                  compileE e1 (argOffset 1 args) <>
                  singleton op'
      Nothing -> compileC inst args
compileE (App (Var "neg") e) args = compileE e args <> singleton Neg
compileE (App (App (Var "add") e1) e2) args
  = compileE e1 args <> compileE e2 args <> singleton Add
compileE (Let isRec defs e) args
  | isRec = unsafePartial (compileLetRec compileE defs e args)
  | otherwise = compileLet compileE defs e args
compileE (Case e alts) args = compileE e args <>
                              (singleton $ Casejump $ compileAlts compileE' alts args)
compileE node@(App e1 e2) args
  | unsafePartial (saturatedCons (makeSpine node)) = compileCS (reverse (makeSpine node)) args
  | otherwise = compileC e2 args <> compileC e1 (argOffset 1 args) <> singleton Mkap
compileE e args = compileC e args <> singleton Eval

makeSpine (App e1 e2) = makeSpine e1 <> singleton e2
makeSpine e = singleton e

saturatedCons :: Partial => List CoreExpr -> Boolean
saturatedCons ((Constr n t a) : es) = a == length es
saturatedCons (e:es) = false

compileCS :: List CoreExpr -> GmEnvironment -> GmCode
compileCS ((Constr n t a) : Nil) _ = singleton $ Pack t a
compileCS (e:es) args = compileC e args <> (compileCS es (argOffset 1 args))
compileCS Nil _ = Nil

compileC :: GmCompiler
compileC = unsafePartial $ compileC'
  where
    compileC' :: Partial => GmCompiler
    compileC' (Num n) _ = singleton $ PushInt n
    compileC' (App e1 e2) args = compileC e2 args <>
                                compileC e1 (argOffset 1 args) <>
                               singleton Mkap
    compileC' (Let isRec defs e) args
      | isRec = unsafePartial $ compileLetRec compileC defs e args
      | otherwise = compileLet compileC defs e args
    compileC' (Var v) args
      | elem v (aDomain args) = singleton $
                                Push (unsafePartial $ fromJust $ lookup v args)
      | otherwise = singleton $ PushGlobal v

compileLet :: GmCompiler -> List (Name /\ CoreExpr) -> GmCompiler
compileLet comp defs expr args
  = compileLet' defs args <> comp expr args' <> singleton (Slide (length defs))
    where args' = compileArgs defs args

compileLet' :: List (Name /\ CoreExpr) -> GmEnvironment -> GmCode
compileLet' Nil _ = Nil
compileLet' (Cons (name /\ expr) defs) args
  = compileC expr args <> compileLet' defs (argOffset 1 args)

compileArgs :: List (Name /\ CoreExpr) -> GmEnvironment -> GmEnvironment
compileArgs defs args
  = zip (fst <$> defs) ((n-1)..0) <> argOffset n args
    where n = length defs

compileLetRec :: Partial => GmCompiler -> List (Name /\ CoreExpr) -> GmCompiler
compileLetRec comp defs expr args
  = singleton (Alloc n) <>
    compiled defs (n-1) <>
    comp expr newArgs <>
    singleton (Slide n)
    where newArgs = compileArgs defs args
          n = length defs
          compiled Nil _ = Nil
          compiled (Cons d ds) i = compileC (snd d) newArgs <>
                                    singleton (Update i) <>
                                    compiled ds (i-1)

compileAlts comp alts args
  = ado { caseTag, vars, rhs } <- alts
        let n = length vars
            args' = zip vars (0..n) <> argOffset n args
    in caseTag /\ (comp n rhs args)

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env
  = ado (v /\ m) <- env
    in (v /\ (n + m))

builtInDyadic :: ASSOC Name Instruction
builtInDyadic = fromFoldable
  [ "add" /\ Add
  , "sub" /\ Sub
  , "mul" /\ Mul
  , "div" /\ Div
  , "eq" /\ Eq
  , "neq" /\ Neq
  , "lt" /\ Lt
  , "le" /\ Leq
  , "gt" /\ Gt
  , "geq" /\ Geq
  ]

compiledPrimitives :: List GmCompiledSC
compiledPrimitives
  = compiledDyadic <>
    (singleton $
      CompSC "neq" 2 $
        fromFoldable [Push 1, Eval, Push 1, Eval, Neq, Update 2, Pop 2, Unwind])
  where
    compiledDyadic
      = ado (op /\ instruction) <- builtInDyadic
        in CompSC op 2 $
              fromFoldable [Push 1, Eval, Push 1, Eval, instruction, Update 2, Pop 2, Unwind]
