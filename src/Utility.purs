module Utility where

import Prelude(($), (<>), (+), show)
import Data.List(List(..))
import Data.Tuple(Tuple(..))

{-- mapAccuml :: forall a b c.(b -> a -> Tuple (b c)) -> b -> List a -> Tuple (b (List a)) --}
mapAccuml f b Nil = Tuple b Nil
mapAccuml f b (Cons x xs)
  = let (Tuple b1 x')  = f b x
        (Tuple b2 xs') = mapAccuml f b1 xs
    in Tuple b $ Cons x' xs'

data NameSupply = MkNS Int

initialNameSupply :: NameSupply
initialNameSupply = MkNS 0

newName :: NameSupply -> String -> (Tuple NameSupply String)
newName (MkNS n) prefix = Tuple (MkNS (n+1)) (prefix <> show n)
