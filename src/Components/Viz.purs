module Components.Viz where

import Prelude

import Data.Either (Either(..), hush)
import Data.List (List(..), (:), (..), length, reverse, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive
import Data.Monoid.Disj
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))

import Components.Carousel (mkCarousel, toCarousel, emptyCarousel)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, empty, element, useState, useEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

import Global (encodeURIComponent)

zip3 :: forall a b c. List a -> List b -> List c -> List (a /\ b /\ c)
zip3 xs ys zs = reverse $ go xs ys zs Nil
  where
  go Nil _ _ acc = acc
  go _ Nil _ acc = acc
  go _ _ Nil acc = acc
  go (a : as) (b : bs) (c : cs) acc = go as bs cs $ (a /\ b /\ c) : acc

mkImgSrc :: String -> String
mkImgSrc imgData =
  "data:image/svg+xml;charset=utf8," <>
  (fromMaybe "" $ encodeURIComponent imgData)

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = do
  carouselEl <- mkCarousel
  component "Viz" \{ prog } -> React.do
    carousel /\ setCarousel <- useState emptyCarousel
    let isUnwindList = (_.isUnwind >>> Disj) <$> prog
    rendered <- useAff prog $ sequence $ renderGmState <$> prog
    useEffect (rendered <#> hush) $ do
      case rendered of
        Just (Right imgs) -> do
          setCarousel $
            const $
            toCarousel $
            zip3 isUnwindList (Additive <$> (0..length imgs)) $
            mkImgSrc <$>
            imgs
        _ -> pure $ unit
      pure $ mempty
    case rendered of
        Just (Right imgs) ->
          pure $
            R.div { children: [ element carouselEl { carousel, setCarousel } ]
                  , className: "viz"
                  }
        _ -> pure $ empty
