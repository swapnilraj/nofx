module Compiler where

import Prelude((<$>), (<>))

import Data.List(List(..), fromFoldable)
import Data.Tuple.Nested

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
