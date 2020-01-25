module Syntax where

import Prelude

import Data.List
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3(..))
import Data.Generic.Rep
import Data.Generic.Rep.Show

type Name = String
type Alter a = List { caseTag :: Int
             , vars :: List a
             , rhs :: PAST a
             , cons :: PAST a -- Constructor
             }

type IsRec = Boolean

type CorePAST = PAST Name
-- Parsed AST
data PAST a
  = Var Name
  | App (PAST a) (PAST a)
  | Constr Name
  | Lam a (PAST a)
  | Func a (List Name) (PAST a)
  | Let
      IsRec
      (List (Tuple Name (PAST a)))
      (PAST a)
  | Case
      (PAST a)
      (Alter a)
  | If (PAST a) (PAST a) (PAST a)
  | Lit Lit
derive instance genericPAST :: Generic (PAST a) _
instance showPAST :: Show a => Show (PAST a) where
  show x = genericShow x

data Lit
  = LInt Int
  | LBool Boolean
derive instance genericLit :: Generic Lit _
instance showLit :: Show Lit where
  show x = genericShow x

