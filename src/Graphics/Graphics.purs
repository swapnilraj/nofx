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
import Data.List ((..), length, reverse, unzip, zip)

import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (unwrap)
import Data.Maybe.First (First(..))
import Data.Foldable (class Foldable, foldMap)
import Data.String.Common (null)

import Effect.Aff (Aff)
import Graphics.Graphviz (Engine(..), Format(..), renderToText)

import Control.Alt ((<|>))

import Partial.Unsafe

import Utility
import Compiler

portRow :: String -> String -> String
portRow port label = "<tr><td port=\"" <> port <> "\">" <> label <> "</td></tr>"

table :: String -> String
table rows = "<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
              (if null rows then "<tr><td></td></tr>" else rows) <>
             "</table>>"

renderGmState :: GmState -> Aff String
renderGmState = renderToText Dot Svg <<< toDot

toDot :: GmState -> String
toDot state =
  let uniqueStack = zip (0..length state.stack) state.stack
      tableC = table $ intercalate " " $ reverse $ (\(id /\ x) -> portRow (show id) ("")) <$> uniqueStack
      stackNodes =
        [ global [ Global.RankDir Global.FromLeft ]
        , node "stack" [ Node.Shape Node.Plain, Node.htmlLabel tableC ]
        ]
      (edges /\ nestedNodes) = unzip $ (\stack' -> edgeToHeap state stack') <$> uniqueStack

      in "strict " <> (toText $ DiGraph $ stackNodes <>
                            (fromFoldable edges) <>
                            (join <<< fromFoldable $ nestedNodes))

edgeToHeap :: GmState -> (Int /\ Addr) -> (Definition /\ Array Definition)
edgeToHeap state stackValue =
  let (id /\ addr) = stackValue
      { globals, heap } = state
      nodeGlobal = swapLookup addr globals
      heapVal = hLookupNoCrash heap addr
      nodeHeap = fst <<< (renderHeapNode state) <$> heapVal
      nodeRef = fromMaybe' (\_ -> unsafeCrashWith "s") (nodeGlobal <|> nodeHeap)
      nodeHeapAttrs = snd <<< (renderHeapNode state) <$> heapVal
      nodeAttrs = fromMaybe [] nodeHeapAttrs

  in
    (("stack:" <> show id) ==> nodeRef) /\ nodeAttrs

renderHeapNode :: GmState -> Node -> (String /\ Array Definition)
renderHeapNode state (NNum n) = (show n) /\ []
renderHeapNode state (NAp a1 a2) =
  let ref = "app" <> show a1 <> show a2
      node' = node ref [ Node.label "@", Node.Shape Node.Plain ]

      a1Global = swapLookup a1 globals
      heapVal1 = hLookupNoCrash heap a1
      nodeHeap1 = fst <<< (renderHeapNode state) <$> heapVal1
      nodeRef1 = fromMaybe' (\_ -> unsafeCrashWith "s") (a1Global <|> nodeHeap1)
      nodeAttrs1 = fromMaybe [] $ snd <<< (renderHeapNode state) <$> heapVal1

      a2Global = swapLookup a2 globals
      heapVal2 = hLookupNoCrash heap a2
      nodeHeap2 = fst <<< (renderHeapNode state) <$> heapVal2
      nodeRef2 = fromMaybe' (\_ -> unsafeCrashWith "s") (a2Global <|> nodeHeap2)
      nodeAttrs2 = fromMaybe [] $ snd <<< (renderHeapNode state) <$> heapVal2

  in (ref /\ ([ node'
             , node nodeRef1 []
             , node nodeRef2 []
             , ref ==> nodeRef1
             , ref ==> nodeRef2
             ] <> nodeAttrs1 <> nodeAttrs2))
  where { globals, heap } = state
renderHeapNode state (NGlobal name _ _) = name /\ [ node name [ Node.Shape Node.Plain ] ]
renderHeapNode state (NInd addr) =
  let heapVal = hLookupNoCrash heap addr
      nodeHeap = fst <<< (renderHeapNode state) <$> heapVal
      nodeRef = fromMaybe' (\_ -> unsafeCrashWith "s") nodeHeap
  in nodeRef /\ [ node nodeRef [ Node.Shape Node.Diamond ] ]
  where { globals, heap } = state
renderHeapNode state (NConstr name tag _) = name /\ []

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
  ((Tuple 52 (NInd 51)) : (Tuple 51 (NNum 6)) : (Tuple 50 (NInd 49)) : (Tuple
  49 (NNum 3)) : (Tuple 48 (NInd 47)) : (Tuple 47 (NNum 2)) : (Tuple 46 (NInd
     45)) : (Tuple 45 (NNum 2)) : (Tuple 44 (NInd 43)) : (Tuple 43 (NNum 3)) :
     (Tuple 42 (NInd 16)) : (Tuple 41 (NConstr 2 Nil)) : (Tuple 40 (NInd 39)) :
     (Tuple 39 (NNum 1)) : (Tuple 38 (NInd 37)) : (Tuple 37 (NNum 2)) : (Tuple
     36 (NInd 35)) : (Tuple 35 (NNum 3)) : (Tuple 34 (NAp 2 33)) : (Tuple 33
        (NAp 32 16)) : (Tuple 32 (NAp 4 25)) : (Tuple 31 (NConstr 3 Nil)) :
        (Tuple 30 (NInd 29)) : (Tuple 29 (NNum 2)) : (Tuple 28 (NInd 27)) :
        (Tuple 27 (NNum 3)) : (Tuple 26 (NAp 2 25)) : (Tuple 25 (NAp 24 16)) :
        (Tuple 24 (NAp 4 19)) : (Tuple 23 (NConstr 3 Nil)) : (Tuple 22 (NInd
        21)) : (Tuple 21 (NNum 3)) : (Tuple 20 (NAp 2 19)) : (Tuple 19 (NAp 18
               16)) : (Tuple 18 (NAp 4 14)) : (Tuple 17 (NConstr 3 Nil)) :
               (Tuple 16 (NNum 1)) : (Tuple 15 (NAp 2 14)) : (Tuple 14 (NNum
               4)) : (Tuple 13 (NGlobal "neq" 2 ((Push 1) : Eval : (Push 1) :
               Eval : Neq : (Update 2) : (Pop 2) : Unwind : Nil))) : (Tuple 12
                            (NGlobal "geq" 2 ((Push 1) : Eval : (Push 1) : Eval
                            : Geq : (Update 2) : (Pop 2) : Unwind : Nil))) :
                            (Tuple 11 (NGlobal "gt" 2 ((Push 1) : Eval : (Push
                            1) : Eval : Gt : (Update 2) : (Pop 2) : Unwind :
                            Nil))) : (Tuple 10 (NGlobal "leq" 2 ((Push 1) :
                            Eval : (Push 1) : Eval : Leq : (Update 2) : (Pop 2)
                            : Unwind : Nil))) : (Tuple 9 (NGlobal "lt" 2 ((Push
                                                1) : Eval : (Push 1) : Eval :
                                                Lt : (Update 2) : (Pop 2) :
                                                Unwind : Nil))) : (Tuple 8
                                                                  (NGlobal
                                                                  "neq" 2
                                                                  ((Push 1) :
                                                                  Eval : (Push
                                                                         1) :
                                                                         Eval :
                                                                         Neq :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         7
                                                                         (NGlobal
                                                                         "eq" 2
                                                                         ((Push
                                                                         1) :
                                                                         Eval :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Eq :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         6
                                                                         (NGlobal
                                                                         "div"
                                                                         2
                                                                         ((Push
                                                                         1) :
                                                                         Eval :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Div :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         5
                                                                         (NGlobal
                                                                         "mul"2
                                                                         ((Push
                                                                         1) :
                                                                         Eval :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Mul :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         4
                                                                         (NGlobal
                                                                         "sub"
                                                                         2
                                                                         ((Push
                                                                         1) :
                                                                         Eval :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Sub :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         3
                                                                         (NGlobal
                                                                         "add"
                                                                         2
                                                                         ((Push
                                                                         1) :
                                                                         Eval :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Add :
                                                                         (Update
                                                                         2) :
                                                                         (Pop
                                                                         2) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         2
                                                                         (NGlobal
                                                                         "fac" 1
                                                                         ((PushInt
                                                                         1) :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Eq :
                                                                         (Casejump
                                                                         ((Tuple
                                                                         2
                                                                         ((Split
                                                                         0) :
                                                                         (PushInt
                                                                         1) :
                                                                         (Slide
                                                                         0) :
                                                                         Nil))
                                                                         :
                                                                         (Tuple
                                                                         3
                                                                         ((Split
                                                                         0) :
                                                                         (PushInt
                                                                         1) :
                                                                         (Push
                                                                         1) :
                                                                         (PushGlobal
                                                                         "sub")
                                                                         : Mkap
                                                                         : Mkap
                                                                         :
                                                                         (PushGlobal
                                                                         "fac")
                                                                         : Mkap
                                                                         : Eval
                                                                         :
                                                                         (Push
                                                                         1) :
                                                                         Eval :
                                                                         Mul :
                                                                         (Slide
                                                                         0) :
                                                                         Nil))
                                                                         :
                                                                         Nil))
                                                                         :
                                                                         (Update
                                                                         1) :
                                                                         (Pop
                                                                         1) :
                                                                         Unwind
                                                                         :
                                                                         Nil)))
                                                                         :
                                                                         (Tuple
                                                                         1
                                                                         (NGlobal
                                                                         "main" 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (14 : 51 : 14 : 15 : Nil),
  stats: 209 }


state2 = {
  code: (Unwind : Nil),
  dump: ((Tuple (Mul : (Slide 0) : (Update 1) : (Pop 1) : Unwind : Nil) (51 : 14 : 15 : Nil)) : (Tuple ((Update 0) : (Pop 0) : Unwind : Nil) (1 : Nil)) : (Tuple (Print : Nil) Nil) : Nil),
  globals: ((Tuple "1" 16) : (Tuple "4" 14) : (Tuple "main" 1) : (Tuple "fac" 2) : (Tuple "add" 3) : (Tuple "sub" 4) : (Tuple "mul" 5) : (Tuple "div" 6) : (Tuple "eq" 7) : (Tuple "neq" 8) : (Tuple "lt" 9) : (Tuple "le" 10) : (Tuple "gt" 11) : (Tuple "geq" 12) : (Tuple "neg" 13) : Nil),
  heap: (Tuple 52 (Tuple (53 : 54 : 55 : 56 : 57 : 58 : 59 : 60 : 61 : 62 : 63
        : 64 : 65 : 66 : 67 : 68 : 69 : 70 : 71 : 72 : 73 : 74 : 75 : 76 : 77 :
        78 : 79 : 80 : 81 : 82 : 83 : 84 : 85 : 86 : 87 : 88 : 89 : 90 : 91 :
        92 : 93 : 94 : 95 : 96 : 97 : 98 : 99 : 100 : Nil) ((Tuple 52 (NInd
                                                           51)) : (Tuple 51
                                                                  (NNum 6)) :
                                                                  (Tuple 50
                                                                  (NInd 49)) :
                                                                  (Tuple 49
                                                                  (NNum 3)) :
                                                                  (Tuple 48
                                                                  (NInd 47)) :
                                                                  (Tuple 47
                                                                  (NNum 2)) :
                                                                  (Tuple 46
                                                                  (NInd 45)) :
                                                                  (Tuple 45
                                                                  (NNum 2)) :
                                                                  (Tuple 44
                                                                  (NInd 43)) :
                                                                  (Tuple 43
                                                                  (NNum 3)) :
                                                                  (Tuple 42
                                                                  (NInd 16)) :
                                                                  (Tuple 41
                                                                  (NConstr 2
                                                                  Nil)) :
                                                                  (Tuple 40
                                                                  (NInd 39)) :
                                                                  (Tuple 39
                                                                  (NNum 1)) :
                                                                  (Tuple 38
                                                                  (NInd 37)) :
                                                                  (Tuple 37
                                                                  (NNum 2)) :
                                                                  (Tuple 36
                                                                  (NInd 35)) :
                                                                  (Tuple 35
                                                                  (NNum 3)) :
                                                                  (Tuple 34
                                                                  (NAp 2 33)) :
                                                                  (Tuple 33
                                                                  (NAp 32 16))
                                                                  : (Tuple 32
                                                                    (NAp 4 25))
                                                                    : (Tuple 31
                                                                      (NConstr
                                                                      3 Nil)) :
                                                                      (Tuple 30
                                                                      (NInd
                                                                      29)) :
                                                                      (Tuple 29
                                                                      (NNum 2))
                                                                      : (Tuple
                                                                        28
                                                                        (NInd
                                                                        27)) :
                                                                        (Tuple
                                                                        27
                                                                        (NNum
                                                                        3)) :
                                                                        (Tuple
                                                                        26 (NAp
                                                                           2
                                                                           25))
                                                                           :
                                                                           (Tuple
                                                                           25
                                                                           (NAp
                                                                           24
                                                                           16))
                                                                           :
                                                                           (Tuple
                                                                           24
                                                                           (NAp
                                                                           4
                                                                           19))
                                                                           :
                                                                           (Tuple
                                                                           23
                                                                           (NConstr
                                                                           3
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           22
                                                                           (NInd
                                                                           21))
                                                                           :
                                                                           (Tuple
                                                                           21
                                                                           (NNum
                                                                           3))
                                                                           :
                                                                           (Tuple
                                                                           20
                                                                           (NAp
                                                                           2
                                                                           19))
                                                                           :
                                                                           (Tuple
                                                                           19
                                                                           (NAp
                                                                           18
                                                                           16))
                                                                           :
                                                                           (Tuple
                                                                           18
                                                                           (NAp
                                                                           4
                                                                           14))
                                                                           :
                                                                           (Tuple
                                                                           17
                                                                           (NConstr
                                                                           3
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           16
                                                                           (NNum
                                                                           1))
                                                                           :
                                                                           (Tuple
                                                                           15
                                                                           (NAp
                                                                           2
                                                                           14))
                                                                           :
                                                                           (Tuple
                                                                           14
                                                                           (NNum
                                                                           4))
                                                                           :
                                                                           (Tuple
                                                                           13
                                                                           (NGlobal
                                                                           "neq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Neq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           12
                                                                           (NGlobal
                                                                           "geq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Geq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           11
                                                                           (NGlobal
                                                                           "gt"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Gt
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           10
                                                                           (NGlobal
                                                                           "leq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Leq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           9
                                                                           (NGlobal
                                                                           "lt"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Lt
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           8
                                                                           (NGlobal
                                                                           "neq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Neq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           7
                                                                           (NGlobal
                                                                           "eq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Eq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           6
                                                                           (NGlobal
                                                                           "div"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Div
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           5
                                                                           (NGlobal
                                                                           "mul"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Mul
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           4
                                                                           (NGlobal
                                                                           "sub"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Sub
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           3
                                                                           (NGlobal
                                                                           "add"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Add
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           2
                                                                           (NGlobal
                                                                           "fac"
                                                                           1
                                                                           ((PushInt
                                                                           1) :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Eq
                                                                           :
                                                                           (Casejump
                                                                           ((Tuple
                                                                           2
                                                                           ((Split
                                                                           0) :
                                                                           (PushInt
                                                                           1) :
                                                                           (Slide
                                                                           0) :
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           3
                                                                           ((Split
                                                                           0) :
                                                                           (PushInt
                                                                           1) :
                                                                           (Push
                                                                           1) :
                                                                           (PushGlobal
                                                                           "sub")
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           (PushGlobal
                                                                           "fac")
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Mul
                                                                           :
                                                                           (Slide
                                                                           0) :
                                                                           Nil))
                                                                           :
                                                                           Nil))
                                                                           :
                                                                           (Update
                                                                           1) :
                                                                           (Pop
                                                                           1) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           1
                                                                           (NGlobal
                                                                           "main" 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (14 : Nil),
  stats: 210 }

state3 = {
  code: (Mul : (Slide 0) : (Update 1) : (Pop 1) : Unwind : Nil),
  dump: ((Tuple ((Update 0) : (Pop 0) : Unwind : Nil) (1 : Nil)) : (Tuple (Print : Nil) Nil) : Nil),
  globals: ((Tuple "1" 16) : (Tuple "4" 14) : (Tuple "main" 1) : (Tuple "fac" 2) : (Tuple "add" 3) : (Tuple "sub" 4) : (Tuple "mul" 5) : (Tuple "div" 6) : (Tuple "eq" 7) : (Tuple "neq" 8) : (Tuple "lt" 9) : (Tuple "le" 10) : (Tuple "gt" 11) : (Tuple "geq" 12) : (Tuple "neg" 13) : Nil),
  heap: (Tuple 52 (Tuple (53 : 54 : 55 : 56 : 57 : 58 : 59 : 60 : 61 : 62 : 63
        : 64 : 65 : 66 : 67 : 68 : 69 : 70 : 71 : 72 : 73 : 74 : 75 : 76 : 77 :
        78 : 79 : 80 : 81 : 82 : 83 : 84 : 85 : 86 : 87 : 88 : 89 : 90 : 91 :
        92 : 93 : 94 : 95 : 96 : 97 : 98 : 99 : 100 : Nil) ((Tuple 52 (NInd
                                                           51)) : (Tuple 51
                                                                  (NNum 6)) :
                                                                  (Tuple 50
                                                                  (NInd 49)) :
                                                                  (Tuple 49
                                                                  (NNum 3)) :
                                                                  (Tuple 48
                                                                  (NInd 47)) :
                                                                  (Tuple 47
                                                                  (NNum 2)) :
                                                                  (Tuple 46
                                                                  (NInd 45)) :
                                                                  (Tuple 45
                                                                  (NNum 2)) :
                                                                  (Tuple 44
                                                                  (NInd 43)) :
                                                                  (Tuple 43
                                                                  (NNum 3)) :
                                                                  (Tuple 42
                                                                  (NInd 16)) :
                                                                  (Tuple 41
                                                                  (NConstr 2
                                                                  Nil)) :
                                                                  (Tuple 40
                                                                  (NInd 39)) :
                                                                  (Tuple 39
                                                                  (NNum 1)) :
                                                                  (Tuple 38
                                                                  (NInd 37)) :
                                                                  (Tuple 37
                                                                  (NNum 2)) :
                                                                  (Tuple 36
                                                                  (NInd 35)) :
                                                                  (Tuple 35
                                                                  (NNum 3)) :
                                                                  (Tuple 34
                                                                  (NAp 2 33)) :
                                                                  (Tuple 33
                                                                  (NAp 32 16))
                                                                  : (Tuple 32
                                                                    (NAp 4 25))
                                                                    : (Tuple 31
                                                                      (NConstr
                                                                      3 Nil)) :
                                                                      (Tuple 30
                                                                      (NInd
                                                                      29)) :
                                                                      (Tuple 29
                                                                      (NNum 2))
                                                                      : (Tuple
                                                                        28
                                                                        (NInd
                                                                        27)) :
                                                                        (Tuple
                                                                        27
                                                                        (NNum
                                                                        3)) :
                                                                        (Tuple
                                                                        26 (NAp
                                                                           2
                                                                           25))
                                                                           :
                                                                           (Tuple
                                                                           25
                                                                           (NAp
                                                                           24
                                                                           16))
                                                                           :
                                                                           (Tuple
                                                                           24
                                                                           (NAp
                                                                           4
                                                                           19))
                                                                           :
                                                                           (Tuple
                                                                           23
                                                                           (NConstr
                                                                           3
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           22
                                                                           (NInd
                                                                           21))
                                                                           :
                                                                           (Tuple
                                                                           21
                                                                           (NNum
                                                                           3))
                                                                           :
                                                                           (Tuple
                                                                           20
                                                                           (NAp
                                                                           2
                                                                           19))
                                                                           :
                                                                           (Tuple
                                                                           19
                                                                           (NAp
                                                                           18
                                                                           16))
                                                                           :
                                                                           (Tuple
                                                                           18
                                                                           (NAp
                                                                           4
                                                                           14))
                                                                           :
                                                                           (Tuple
                                                                           17
                                                                           (NConstr
                                                                           3
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           16
                                                                           (NNum
                                                                           1))
                                                                           :
                                                                           (Tuple
                                                                           15
                                                                           (NAp
                                                                           2
                                                                           14))
                                                                           :
                                                                           (Tuple
                                                                           14
                                                                           (NNum
                                                                           4))
                                                                           :
                                                                           (Tuple
                                                                           13
                                                                           (NGlobal
                                                                           "neq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Neq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           12
                                                                           (NGlobal
                                                                           "geq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Geq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           11
                                                                           (NGlobal
                                                                           "gt"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Gt
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           10
                                                                           (NGlobal
                                                                           "leq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Leq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           9
                                                                           (NGlobal
                                                                           "lt"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Lt
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           8
                                                                           (NGlobal
                                                                           "neq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Neq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           7
                                                                           (NGlobal
                                                                           "eq"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Eq
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           6
                                                                           (NGlobal
                                                                           "div"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Div
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           5
                                                                           (NGlobal
                                                                           "mul"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Mul
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           4
                                                                           (NGlobal
                                                                           "sub"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Sub
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           3
                                                                           (NGlobal
                                                                           "add"
                                                                           2
                                                                           ((Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Add
                                                                           :
                                                                           (Update
                                                                           2) :
                                                                           (Pop
                                                                           2) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           2
                                                                           (NGlobal
                                                                           "fac"
                                                                           1
                                                                           ((PushInt
                                                                           1) :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           : Eq
                                                                           :
                                                                           (Casejump
                                                                           ((Tuple
                                                                           2
                                                                           ((Split
                                                                           0) :
                                                                           (PushInt
                                                                           1) :
                                                                           (Slide
                                                                           0) :
                                                                           Nil))
                                                                           :
                                                                           (Tuple
                                                                           3
                                                                           ((Split
                                                                           0) :
                                                                           (PushInt
                                                                           1) :
                                                                           (Push
                                                                           1) :
                                                                           (PushGlobal
                                                                           "sub")
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           (PushGlobal
                                                                           "fac")
                                                                           :
                                                                           Mkap
                                                                           :
                                                                           Eval
                                                                           :
                                                                           (Push
                                                                           1) :
                                                                           Eval
                                                                           :
                                                                           Mul
                                                                           :
                                                                           (Slide
                                                                           0) :
                                                                           Nil))
                                                                           :
                                                                           Nil))
                                                                           :
                                                                           (Update
                                                                           1) :
                                                                           (Pop
                                                                           1) :
                                                                           Unwind
                                                                           :
                                                                           Nil)))
                                                                           :
                                                                           (Tuple
                                                                           1
                                                                           (NGlobal
                                                                           "main" 0 ((PushInt 4) : (PushGlobal "fac") : Mkap : Eval : (Update 0) : (Pop 0) : Unwind : Nil))) : Nil))),
  output: "",
  stack: (22 : 51 : 14 : 15 : Nil),
  stats: 211 }
