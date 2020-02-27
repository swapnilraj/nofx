module Utility where

import Prelude (($), (<>), (+), (-), (<=), show, otherwise)
import Data.List (List(..), (:), (..), length, null, singleton, zip, zipWith)
import Data.Tuple (Tuple(..), lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested

import Partial.Unsafe (unsafeCrashWith, unsafePartial)
{-- mapAccuml :: forall a b c.(b -> a -> Tuple (b c)) -> b -> List a -> Tuple (b (List a)) --}
mapAccuml f b Nil = Tuple b Nil

mapAccuml f b (x:xs) =
  let
    (b1 /\ x') = f b x
    (b2 /\ xs') = mapAccuml f b1 xs
  in b2 /\ (x':xs')

type Addr = Int
type Heap a = Int /\ List Int /\ List (Int /\ a)

data NameSupply = MkNS Int

type ASSOC a b = List (Tuple a b)

initialNameSupply :: NameSupply
initialNameSupply = MkNS 0

newName :: NameSupply -> String -> (Tuple NameSupply String)
newName (MkNS n) prefix = Tuple (MkNS (n + 1)) (prefix <> show n)

makeName prefix ns = prefix <> "_" <> show ns

getNames :: NameSupply -> List String -> Tuple NameSupply (List String)
getNames (MkNS n) prefixes = MkNS (n + len) /\ zipWith makeName prefixes (n .. (n + len))
  where
  len = length prefixes

newNames :: NameSupply -> List String -> (NameSupply /\ List String /\ ASSOC String String)
newNames ns oldNames = ns' /\ newNames /\ env
  where
  (ns' /\ newNames) = getNames ns oldNames
  env = zip oldNames newNames

emptyList :: forall a .(a -> Boolean) -> a -> a -> a
emptyList f list b = if f list then b else list

splitAt :: forall a.Int -> List a -> ((List a) /\ (List a))
splitAt n ls
  | n <= 0 = (Nil /\ ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: forall a.Int -> List a -> ((List a) /\ (List a))
        splitAt' _  Nil     = (Nil /\ Nil)
        splitAt' 1  (x:xs) = (singleton x /\ xs)
        splitAt' m  (x:xs) = ((x:xs') /\ xs'')
          where
            (xs' /\ xs'') = splitAt' (m - 1) xs

-------------------------------------------------------------------------------

hAlloc :: forall a. Heap a -> a -> (Heap a /\ Addr)
hAlloc = unsafePartial $ hAlloc'
  where
    hAlloc' :: Partial => forall a. Heap a -> a -> (Heap a /\ Addr)
    hAlloc' (size /\ (next:free) /\ cts) n =
        (((size+1) /\ (free /\ (next /\ n) : cts)) /\ next)

hInitial = (0 /\ (1..100) /\ Nil)

hNull = 0

hLookup :: forall a.Heap a -> Int -> a
hLookup (size /\ free /\ cts) a =
  case lookup a cts of
       Just h -> h
       Nothing -> unsafeCrashWith ("not found node" <> show a)
