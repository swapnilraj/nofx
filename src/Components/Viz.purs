module Components.Viz where

import Prelude

import Data.Array (fromFoldable)
import Data.List (List(..), head, singleton, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, empty, element, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

import Global (encodeURIComponent)

import Debug.Trace

data Carousel a = Carousel (List a) a (List a)

toCarousel :: forall a.Monoid a => List a -> Carousel a
toCarousel l = Carousel Nil hd tl
  where hd = fromMaybe mempty $ head l
        tl = fromMaybe Nil $ tail l

nextCarousel :: forall a.Monoid a => Carousel a -> Carousel a
nextCarousel (Carousel bef curr after) =
  Carousel (bef <> singleton curr) curr' tl
  where curr' = fromMaybe mempty $ head after
        tl = fromMaybe Nil $ tail after

emptyCarousel :: Carousel String
emptyCarousel = (Carousel Nil "" Nil)

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = do
  carouseImg <- mkCarouselImg
  component "Viz" \{ prog } -> React.do
    carousel /\ setCarousel <- useState emptyCarousel
    rendered <- useAff prog (sequence $ renderGmState <$> prog)
    case rendered of
        Just (Right imgs) -> do
          let c = toCarousel $ mkImgSrc <$> imgs
          _ <- pure $ setCarousel (\_ -> c)
          _ <- trace imgs (\_ -> pure $ empty)
          _ <- trace carousel (\_ -> pure $ empty)
          _ <- trace (c) (\_ -> pure $ empty)
          pure $ R.div_ [ element carouseImg { carousel:  carousel } ]
        _ -> pure $ empty

mkImgSrc :: String -> String
mkImgSrc imgData =
  "data:image/svg+xml;charset=utf8," <> (fromMaybe "" $ encodeURIComponent imgData)

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel String } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ c _) = carousel
                pure $ R.img { src: c }
