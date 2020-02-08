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
  | Cond GmCode GmCode
  | Pack Int Int
  | Casejump (List Int /\ GmCode)
  | Split Int

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int (List Addr)
