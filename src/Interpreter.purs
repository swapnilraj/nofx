module Interpreter where

import Prelude ((+), (<>), ($), otherwise)

import Data.List (List(..), (:), null)
import Data.Maybe (Maybe(..))
import Data.Tuple (lookup)

import Partial.Unsafe(unsafeCrashWith)

import AST
import Compiler

interpret :: GmState -> List GmState
interpret s = s:ss
  where nextState = countSteps (step s)
        ss | null s.code = Nil
           | otherwise = interpret nextState

countSteps :: GmState -> GmState
countSteps s = s { stats = s.stats + 1 }

step :: GmState -> GmState
step s = go s.code
  where
    go (Cons i is) = dispatch i (s { code = is})
    go Nil = unsafeCrashWith "No code to execute"

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f) = pushglobal f
dispatch _ = unsafeCrashWith "Not implemented yet"

pushglobal :: Name -> GmState -> GmState
pushglobal f s =
  let global = case lookup f (s.globals) of
                    Just g -> g
                    Nothing -> unsafeCrashWith $ "Undeclared global " <> f
  in s { stack = s.stack }
