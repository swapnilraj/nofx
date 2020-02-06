module Utility where

import Prelude(($), (<>), (+), show)
import Data.List(List(..), (..), length, zip, zipWith)
import Data.Tuple(Tuple(..))
import Data.Tuple.Nested

{-- mapAccuml :: forall a b c.(b -> a -> Tuple (b c)) -> b -> List a -> Tuple (b (List a)) --}
mapAccuml f b Nil = Tuple b Nil
mapAccuml f b (Cons x xs)
  = let (Tuple b1 x')  = f b x
        (Tuple b2 xs') = mapAccuml f b1 xs
    in Tuple b $ Cons x' xs'

data NameSupply = MkNS Int

type ASSOC a b = List (Tuple a b)

initialNameSupply :: NameSupply
initialNameSupply = MkNS 0

newName :: NameSupply -> String -> (Tuple NameSupply String)
newName (MkNS n) prefix = Tuple (MkNS (n+1)) (prefix <> show n)

makeName prefix ns = prefix <> "_" <> show ns

getNames :: NameSupply -> List String -> Tuple NameSupply (List String)
getNames (MkNS n) prefixes
  = MkNS (n + len) /\ zipWith makeName prefixes (n..(n + len))
    where len = length prefixes

newNames :: NameSupply -> List String -> (NameSupply /\ List String /\ ASSOC String String)
newNames ns oldNames
  = ns'/\ newNames /\ env
    where
    (ns'/\ newNames) = getNames ns oldNames
    env = zip oldNames newNames
