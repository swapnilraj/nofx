module Graphics where

import Prelude

import Data.List (List(..), (:), concatMap)

import Data.Foldable (intercalate)
import Data.DotLang (class GraphRepr, toGraph)
import Data.DotLang.Class (toText)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)

import Data.DotLang (Definition(..), Graph(..), Edge(..), EdgeType(..), global, node, (==>), (=*>))
import Data.DotLang.Class (toText)
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Attr.Global as Global

import Data.Array (fromFoldable)
import Data.Tuple
import Data.Tuple.Nested
import Data.List ((..), length, unzip, zip)

import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (unwrap)
import Data.Maybe.First (First(..))
import Data.Foldable (class Foldable, foldMap)

import Control.Alt ((<|>))

import Partial.Unsafe

import Utility
import Compiler

portRow port label = "<tr><td port=\"" <> port <> "\">" <> label <> "</td></tr>"

table rows = "<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
              rows <>
             "</table>>"

doMyThing =
  let uniqueStack = zip (0..length state.stack) state.stack
      tableC = table $ intercalate " " $ (\(id /\ x) -> portRow (show id) (show x)) <$> uniqueStack
      stackNodes =
        [ global [ Global.RankDir Global.FromLeft ]
        , node "stack" [ Node.Shape Node.Plain, Node.htmlLabel tableC ]
        ]
      (edges /\ nestedNodes) = unzip $ (\stack' -> edgeToHeap state stack') <$> uniqueStack

      in toText $ DiGraph $ stackNodes <>
                            (fromFoldable edges) <>
                            (join <<< fromFoldable $ nestedNodes)

edgeToHeap :: GmState -> (Int /\ Addr) -> (Definition /\ Array Definition)
edgeToHeap { globals, heap } stackValue =
  let (id /\ addr) = stackValue
      nodeGlobal = swapLookup addr globals
      heapVal = hLookupNoCrash heap addr
      nodeHeap = fst <<< renderHeapNode <$> heapVal
      nodeRef = fromMaybe' (\_ -> unsafeCrashWith "s") (nodeGlobal <|> nodeHeap)
      nodeHeapAttrs = snd <<< renderHeapNode <$> heapVal
      nodeAttrs = fromMaybe [] nodeHeapAttrs

  in
    (("stack:" <> show id) ==> nodeRef) /\ nodeAttrs

renderHeapNode :: Node -> (String /\ Array Definition)
renderHeapNode (NNum n) = (show n) /\ []
renderHeapNode (NAp a1 a2) =
  let ref = show a1 <> show a2
      node' = node ref [ Node.label "@" ]
  in (ref /\ [ node'
             , ref ==> "a1"
             , ref ==> "a2"
             ])
renderHeapNode (NGlobal _ _) = "fac" /\ []
renderHeapNode (NInd addr) = "Ind" /\ []
renderHeapNode (NConstr tag _) = "Cons" /\ []

hLookupNoCrash :: forall a.Heap a -> Int -> Maybe a
hLookupNoCrash (size /\ free /\ cts) a = lookup a cts

swapLookup :: forall a b f. Foldable f => Eq b => b -> f (a /\ b) -> Maybe a
swapLookup b =
  unwrap <<< foldMap \(a /\ b') -> First (if b == b' then Just a else Nothing)

--------------------------------------------------------------------------------
{-- Debugging Data --}

state = {
  code: (Eval : Mul : (Slide 0) : (Update 1) : (Pop 1) : Unwind : Nil),
  dump: ((Tuple ((Update 0) : (Pop 0) : Unwind : Nil) (1 : Nil)) : (Tuple (Print : Nil) Nil) : Nil),
  globals: ((Tuple "1" 16) : (Tuple "4" 14) : (Tuple "main" 1) : (Tuple "fac" 2) : (Tuple "add" 3) : (Tuple "sub" 4) : (Tuple "mul" 5) : (Tuple "div" 6) : (Tuple "eq" 7) : (Tuple "neq" 8) : (Tuple "lt" 9) : (Tuple "le" 10) : (Tuple "gt" 11) : (Tuple "geq" 12) : (Tuple "neg" 13) : Nil),
  heap: (Tuple 52
        (Tuple (53 : 54 : 55 : 56 : 57 : 58 : 59 : 60 : 61 : 62 : 63 : 64 : 65 : 66 : 67 : 68 : 69 : 70 : 71 : 72 : 73 : 74 : 75 : 76 : 77 : 78 : 79 : 80 : 81 : 82 : 83 : 84 : 85 : 86 : 87 : 88 : 89 : 90 : 91 : 92 : 93 : 94 : 95 : 96 : 97 : 98 : 99 : 100 : Nil)
  ((Tuple 52 (NInd 51)) : (Tuple 51 (NNum 6)) : (Tuple 50 (NInd 49)) : (Tuple 49 (NNum 3)) : (Tuple 48 (NInd 47)) : (Tuple 47 (NNum 2)) : (Tuple 46 (NInd 45)) : (Tuple 45 (NNum 2)) : (Tuple 44 (NInd 43)) : (Tuple 43 (NNum 3)) : (Tuple 42 (NInd 16)) : (Tuple 41 (NConstr 2 Nil)) : (Tuple 40 (NInd 39)) : (Tuple 39 (NNum 1)) : (Tuple 38 (NInd 37)) : (Tuple 37 (NNum 2)) : (Tuple 36 (NInd 35)) : (Tuple 35 (NNum 3)) : (Tuple 34 (NAp 2 33)) : (Tuple 33 (NAp 32 16)) : (Tuple 32 (NAp 4 25)) : (Tuple 31 (NConstr 3 Nil)) : (Tuple 30 (NInd 29)) : (Tuple 29 (NNum 2)) : (Tuple 28 (NInd 27)) : (Tuple 27 (NNum 3)) : (Tuple 26 (NAp 2 25)) : (Tuple 25 (NAp 24 16)) : (Tuple 24 (NAp 4 19)) : (Tuple 23 (NConstr 3 Nil)) : (Tuple 22 (NInd 21)) : (Tuple 21 (NNum 3)) : (Tuple 20 (NAp 2 19)) : (Tuple 19 (NAp 18 16)) : (Tuple 18 (NAp 4 14)) : (Tuple 17 (NConstr 3 Nil)) : (Tuple 16 (NNum 1)) : (Tuple 15 (NAp 2 14)) : (Tuple 14 (NNum 4)) : (Tuple 13 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 12 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Geq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 11 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Gt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 10 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Leq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 9 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Lt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 8 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 7 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Eq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 6 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Div : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 5 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Mul : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 4 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Sub : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 3 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Add : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 2 (NGlobal 1 ((PushInt 1) : (Push 1) : Eval : Eq : (Casejump ((Tuple 2 ((Split 0) : (PushInt 1) : (Slide 0) : Nil)) : (Tuple 3 ((Split 0) : (PushInt 1) : (Push 1) : (PushGlobal "sub") : Mkap : Mkap : (PushGlobal "fac") : Mkap : Eval : (Push 1) : Eval : Mul : (Slide 0) : Nil)) : Nil)) : (Update 1) : (Pop 1) : Unwind : Nil))) : (Tuple 1 (NGlobal 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (14 : 51 : 14 : 15 : Nil),
  stats: 209 }


state2 = {
  code: (Unwind : Nil),
  dump: ((Tuple (Mul : (Slide 0) : (Update 1) : (Pop 1) : Unwind : Nil) (51 : 14 : 15 : Nil)) : (Tuple ((Update 0) : (Pop 0) : Unwind : Nil) (1 : Nil)) : (Tuple (Print : Nil) Nil) : Nil),
  globals: ((Tuple "1" 16) : (Tuple "4" 14) : (Tuple "main" 1) : (Tuple "fac" 2) : (Tuple "add" 3) : (Tuple "sub" 4) : (Tuple "mul" 5) : (Tuple "div" 6) : (Tuple "eq" 7) : (Tuple "neq" 8) : (Tuple "lt" 9) : (Tuple "le" 10) : (Tuple "gt" 11) : (Tuple "geq" 12) : (Tuple "neg" 13) : Nil),
  heap: (Tuple 52 (Tuple (53 : 54 : 55 : 56 : 57 : 58 : 59 : 60 : 61 : 62 : 63 : 64 : 65 : 66 : 67 : 68 : 69 : 70 : 71 : 72 : 73 : 74 : 75 : 76 : 77 : 78 : 79 : 80 : 81 : 82 : 83 : 84 : 85 : 86 : 87 : 88 : 89 : 90 : 91 : 92 : 93 : 94 : 95 : 96 : 97 : 98 : 99 : 100 : Nil) ((Tuple 52 (NInd 51)) : (Tuple 51 (NNum 6)) : (Tuple 50 (NInd 49)) : (Tuple 49 (NNum 3)) : (Tuple 48 (NInd 47)) : (Tuple 47 (NNum 2)) : (Tuple 46 (NInd 45)) : (Tuple 45 (NNum 2)) : (Tuple 44 (NInd 43)) : (Tuple 43 (NNum 3)) : (Tuple 42 (NInd 16)) : (Tuple 41 (NConstr 2 Nil)) : (Tuple 40 (NInd 39)) : (Tuple 39 (NNum 1)) : (Tuple 38 (NInd 37)) : (Tuple 37 (NNum 2)) : (Tuple 36 (NInd 35)) : (Tuple 35 (NNum 3)) : (Tuple 34 (NAp 2 33)) : (Tuple 33 (NAp 32 16)) : (Tuple 32 (NAp 4 25)) : (Tuple 31 (NConstr 3 Nil)) : (Tuple 30 (NInd 29)) : (Tuple 29 (NNum 2)) : (Tuple 28 (NInd 27)) : (Tuple 27 (NNum 3)) : (Tuple 26 (NAp 2 25)) : (Tuple 25 (NAp 24 16)) : (Tuple 24 (NAp 4 19)) : (Tuple 23 (NConstr 3 Nil)) : (Tuple 22 (NInd 21)) : (Tuple 21 (NNum 3)) : (Tuple 20 (NAp 2 19)) : (Tuple 19 (NAp 18 16)) : (Tuple 18 (NAp 4 14)) : (Tuple 17 (NConstr 3 Nil)) : (Tuple 16 (NNum 1)) : (Tuple 15 (NAp 2 14)) : (Tuple 14 (NNum 4)) : (Tuple 13 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 12 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Geq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 11 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Gt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 10 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Leq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 9 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Lt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 8 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 7 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Eq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 6 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Div : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 5 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Mul : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 4 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Sub : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 3 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Add : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 2 (NGlobal 1 ((PushInt 1) : (Push 1) : Eval : Eq : (Casejump ((Tuple 2 ((Split 0) : (PushInt 1) : (Slide 0) : Nil)) : (Tuple 3 ((Split 0) : (PushInt 1) : (Push 1) : (PushGlobal "sub") : Mkap : Mkap : (PushGlobal "fac") : Mkap : Eval : (Push 1) : Eval : Mul : (Slide 0) : Nil)) : Nil)) : (Update 1) : (Pop 1) : Unwind : Nil))) : (Tuple 1 (NGlobal 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (14 : Nil),
  stats: 210 }

state3 = {
  code: (Mul : (Slide 0) : (Update 1) : (Pop 1) : Unwind : Nil),
  dump: ((Tuple ((Update 0) : (Pop 0) : Unwind : Nil) (1 : Nil)) : (Tuple (Print : Nil) Nil) : Nil),
  globals: ((Tuple "1" 16) : (Tuple "4" 14) : (Tuple "main" 1) : (Tuple "fac" 2) : (Tuple "add" 3) : (Tuple "sub" 4) : (Tuple "mul" 5) : (Tuple "div" 6) : (Tuple "eq" 7) : (Tuple "neq" 8) : (Tuple "lt" 9) : (Tuple "le" 10) : (Tuple "gt" 11) : (Tuple "geq" 12) : (Tuple "neg" 13) : Nil),
  heap: (Tuple 52 (Tuple (53 : 54 : 55 : 56 : 57 : 58 : 59 : 60 : 61 : 62 : 63 : 64 : 65 : 66 : 67 : 68 : 69 : 70 : 71 : 72 : 73 : 74 : 75 : 76 : 77 : 78 : 79 : 80 : 81 : 82 : 83 : 84 : 85 : 86 : 87 : 88 : 89 : 90 : 91 : 92 : 93 : 94 : 95 : 96 : 97 : 98 : 99 : 100 : Nil) ((Tuple 52 (NInd 51)) : (Tuple 51 (NNum 6)) : (Tuple 50 (NInd 49)) : (Tuple 49 (NNum 3)) : (Tuple 48 (NInd 47)) : (Tuple 47 (NNum 2)) : (Tuple 46 (NInd 45)) : (Tuple 45 (NNum 2)) : (Tuple 44 (NInd 43)) : (Tuple 43 (NNum 3)) : (Tuple 42 (NInd 16)) : (Tuple 41 (NConstr 2 Nil)) : (Tuple 40 (NInd 39)) : (Tuple 39 (NNum 1)) : (Tuple 38 (NInd 37)) : (Tuple 37 (NNum 2)) : (Tuple 36 (NInd 35)) : (Tuple 35 (NNum 3)) : (Tuple 34 (NAp 2 33)) : (Tuple 33 (NAp 32 16)) : (Tuple 32 (NAp 4 25)) : (Tuple 31 (NConstr 3 Nil)) : (Tuple 30 (NInd 29)) : (Tuple 29 (NNum 2)) : (Tuple 28 (NInd 27)) : (Tuple 27 (NNum 3)) : (Tuple 26 (NAp 2 25)) : (Tuple 25 (NAp 24 16)) : (Tuple 24 (NAp 4 19)) : (Tuple 23 (NConstr 3 Nil)) : (Tuple 22 (NInd 21)) : (Tuple 21 (NNum 3)) : (Tuple 20 (NAp 2 19)) : (Tuple 19 (NAp 18 16)) : (Tuple 18 (NAp 4 14)) : (Tuple 17 (NConstr 3 Nil)) : (Tuple 16 (NNum 1)) : (Tuple 15 (NAp 2 14)) : (Tuple 14 (NNum 4)) : (Tuple 13 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 12 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Geq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 11 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Gt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 10 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Leq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 9 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Lt : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 8 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 7 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Eq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 6 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Div : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 5 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Mul : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 4 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Sub : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 3 (NGlobal 2 ((Push 1) : Eval : (Push 1) : Eval : Add : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 2 (NGlobal 1 ((PushInt 1) : (Push 1) : Eval : Eq : (Casejump ((Tuple 2 ((Split 0) : (PushInt 1) : (Slide 0) : Nil)) : (Tuple 3 ((Split 0) : (PushInt 1) : (Push 1) : (PushGlobal "sub") : Mkap : Mkap : (PushGlobal "fac") : Mkap : Eval : (Push 1) : Eval : Mul : (Slide 0) : Nil)) : Nil)) : (Update 1) : (Pop 1) : Unwind : Nil))) : (Tuple 1 (NGlobal 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (14 : 51 : 14 : 15 : Nil),
  stats: 211 }
