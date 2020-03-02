module Syntax where

import Prelude
import Data.List
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3(..))
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Generic.Rep.Eq

type Name = String

type Alter a
  = List
      { cons :: PAST a -- Constructor
      , rhs :: PAST a
      }

type IsRec = Boolean

type CorePAST = PAST Name

-- Parsed AST
data PAST a
  = Var Name
  | App (PAST a) (PAST a)
  | Constr Name
  | Lam (List a) (PAST a)
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

instance eqPAST :: Eq a => Eq (PAST a) where
  eq x = genericEq x

data Lit
  = LInt Int
  | LBool Boolean

derive instance genericLit :: Generic Lit _

instance showLit :: Show Lit where
  show x = genericShow x

instance eqLit :: Eq Lit where
  eq x = genericEq x

type Program
  = List CorePSC

type CorePSC
  = PSC Name

-- Parsed supercombinators
data PSC a
  = Func Name (List a) (PAST a)

derive instance genericPSC :: Generic (PSC a) _

instance showPSC :: Show a => Show (PSC a) where
  show x = genericShow x
