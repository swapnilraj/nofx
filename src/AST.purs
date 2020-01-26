module AST where

import Prelude

import Data.List
import Data.Tuple (Tuple(..))
import Data.Generic.Rep
import Data.Generic.Rep.Show

type Name = String
type Alter a = List { caseTag :: Int
             , vars :: List a
             , rhs :: Expr a
             , cons :: Expr a -- Constructor
             }

type IsRec = Boolean

type CoreExpr = Expr Name
-- Expression with all language constructs
data Expr a
  = Var Name
  | Num Int
  | App (Expr a) (Expr a)
  | Constr
      Name
      Int -- Tag
      Int -- Arity of constructor
  | Lam a (Expr a)
  | Let
      IsRec
      (List (Tuple Name (Expr a)))
      (Expr a)
  | Case
      (Expr a)
      (Alter a)
derive instance genericExpr :: Generic (Expr a) _
instance showExpr :: Show a => Show (Expr a) where
  show x = genericShow x

type CoreSC = SC Name
data SC a = Func Name (List a) (Expr a)
derive instance genericSC :: Generic (SC a) _
instance showSC :: Show a => Show (SC a) where
  show x = genericShow x
