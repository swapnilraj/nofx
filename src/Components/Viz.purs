module Components.Viz where

import Prelude

import Data.List (List(..), head, init, last, singleton, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), hush)
import Data.Traversable (sequence)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, empty, element, useState, useEffect, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

import Global (encodeURIComponent)

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

previousCarousel :: forall a.Monoid a => Carousel a -> Carousel a
previousCarousel (Carousel bef curr after) =
  Carousel ini lt (Cons curr after)
  where ini = fromMaybe Nil $ init after
        lt = fromMaybe mempty $ last bef

emptyCarousel :: Carousel String
emptyCarousel = (Carousel Nil "" Nil)

mkImgSrc :: String -> String
mkImgSrc imgData =
  "data:image/svg+xml;charset=utf8," <>
  (fromMaybe "" $ encodeURIComponent imgData)

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel String } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ c _) = carousel
                pure $ R.img { src: c }

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = do
  carouselImg <- mkCarouselImg
  carouselControl <- mkCarouselControl
  component "Viz" \{ prog } -> React.do
    carousel /\ setCarousel <- useState emptyCarousel
    rendered <- useAff prog (sequence $ renderGmState <$> prog)
    useEffect (rendered <#> hush) $ do
      case rendered of
        Just (Right imgs) -> do
          setCarousel $ const $ toCarousel $ mkImgSrc <$> imgs
        _ -> pure $ unit
      pure $ mempty
    case rendered of
        Just (Right imgs) ->
          pure $ R.div_ [ element carouselImg { carousel }
                        , element carouselControl { carousel, setCarousel }
                        ]
        _ -> pure $ empty

type CarouselAction = forall a. Monoid a => Effect (ReactComponent
                        { carousel :: Carousel a
                        , setCarousel :: (Carousel a -> Carousel a) -> Effect Unit
                        })

mkCarouselControl :: CarouselAction
mkCarouselControl = do
  nextButton <- mkNextButton
  previousButton <- mkPreviousButton
  component "CarouselControl" \props -> React.do
     pure $ R.div_ [ element nextButton props
                   , element previousButton props
                   ]

mkPreviousButton :: CarouselAction
mkPreviousButton = component "PreviousButton" \{ carousel, setCarousel } -> React.do
                let previous = previousCarousel carousel
                pure $ R.button { children: [ R.text "Previous" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextButton :: CarouselAction
mkNextButton = component "NextButton" \{ carousel, setCarousel } -> React.do
                let next = nextCarousel carousel
                pure $ R.button { children: [ R.text "Next" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }
