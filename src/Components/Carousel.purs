module Components.Carousel where

import Prelude

import Data.List (List(..), head, init, last, length, singleton, tail)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Monoid.Additive

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, empty)

data Carousel a = Carousel (List a) a (List a)

toCarousel :: forall a.Monoid a => List a -> Carousel a
toCarousel l = Carousel Nil hd tl
  where hd = fromMaybe mempty $ head l
        tl = fromMaybe Nil $ tail l

nextCarousel :: forall a.Monoid a => Carousel a -> Carousel a
nextCarousel c@(Carousel _ _ Nil) = c
nextCarousel (Carousel bef curr after) =
  Carousel (bef <> singleton curr) curr' tl
  where curr' = fromMaybe mempty $ head after
        tl = fromMaybe Nil $ tail after

previousCarousel :: forall a.Monoid a => Carousel a -> Carousel a
previousCarousel c@(Carousel Nil _ _) = c
previousCarousel (Carousel bef curr after) =
  Carousel ini lt (Cons curr after)
  where ini = fromMaybe Nil $ init bef
        lt = fromMaybe mempty $ last bef

type CarouselAction a = Effect (ReactComponent
                        { carousel :: Carousel a
                        , setCarousel :: (Carousel a -> Carousel a) -> Effect Unit
                        })

mkCarousel :: CarouselAction (Additive Int /\ String)
mkCarousel = do
  carouselImg <- mkCarouselImg
  carouselControl <- mkCarouselControl
  component "Carousel" \{ carousel, setCarousel } -> React.do
    let (Carousel _ curr _ ) = carousel
    pure $ if curr /= mempty then
           R.div { children: [ element carouselImg { carousel }
                             , element carouselControl { carousel, setCarousel }
                             ]
                 , className: "carousel"
                 }
          else empty

emptyCarousel :: forall a. Monoid a => Carousel a
emptyCarousel = (Carousel Nil mempty Nil)

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel (Additive Int /\ String) } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ (_ /\ c) _) = carousel
                pure $ R.div { children: [ R.img { src: c
                                                 , className: "carousel-img"
                                                 }
                                         ]
                             , className: "carousel-img-container"
                             }

mkCarouselControl :: CarouselAction (Additive Int /\ String)
mkCarouselControl = do
  carouselConuter <- mkCarouselCounter
  nextButton <- mkNextButton
  previousButton <- mkPreviousButton
  component "CarouselControl" \props -> React.do
     pure $ R.div { children: [ element carouselConuter props
                              , R.div { children: [ element previousButton props
                                                  , element nextButton props
                                                  ]
                                      , className: "carousel-control"
                                      }
                              ]
                  , className: "carousel-info"
                  }

mkCarouselCounter :: CarouselAction (Additive Int /\ String)
mkCarouselCounter = component "CarouselCounter" \{ carousel } -> React.do
                    let (Carousel before (Additive curr /\ _) after ) = carousel
                        total = length before + length after
                    pure $ R.text $ show curr <> " / " <> show total

mkPreviousButton :: CarouselAction (Additive Int /\ String)
mkPreviousButton = component "PreviousButton" \{ carousel, setCarousel } -> React.do
                let previous = previousCarousel carousel
                pure $ R.button { children: [ R.text "Previous" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextButton :: CarouselAction (Additive Int /\ String)
mkNextButton = component "NextButton" \{ carousel, setCarousel } -> React.do
                let next = nextCarousel carousel
                pure $ R.button { children: [ R.text "Next" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }
