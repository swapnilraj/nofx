module Components.Carousel where

import Prelude

import Data.List (List(..), head, init, last, length, singleton, tail)
import Data.Maybe (fromMaybe)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, empty)

data Carousel a = Carousel (List a) a Int Int (List a)

toCarousel :: forall a.Monoid a => List a -> Carousel a
toCarousel l = Carousel Nil hd 0 (length l - 1) tl
  where hd = fromMaybe mempty $ head l
        tl = fromMaybe Nil $ tail l

nextCarousel :: forall a.Monoid a => Carousel a -> Carousel a
nextCarousel c@(Carousel _ _ _ _ Nil) = c
nextCarousel (Carousel bef curr n bound after) =
  Carousel (bef <> singleton curr) curr' (n + 1) bound tl
  where curr' = fromMaybe mempty $ head after
        tl = fromMaybe Nil $ tail after

previousCarousel :: forall a.Monoid a => Carousel a -> Carousel a
previousCarousel c@(Carousel Nil _ _ _ _) = c
previousCarousel (Carousel bef curr n bound after) =
  Carousel ini lt (n - 1) bound (Cons curr after)
  where ini = fromMaybe Nil $ init bef
        lt = fromMaybe mempty $ last bef

type CarouselAction =
  forall a. Monoid a => Effect (ReactComponent
                        { carousel :: Carousel a
                        , setCarousel :: (Carousel a -> Carousel a) -> Effect Unit
                        })

mkCarousel :: Effect (ReactComponent
                      { carousel :: Carousel String
                      , setCarousel :: (Carousel String -> Carousel String) -> Effect Unit
                      })
mkCarousel = do
  carouselImg <- mkCarouselImg
  carouselControl <- mkCarouselControl
  component "Carousel" \{ carousel, setCarousel } -> React.do
    let (Carousel _ _ _ total _ ) = carousel
    pure $ if total > 0 then
           R.div { children: [ element carouselImg { carousel }
                             , element carouselControl { carousel, setCarousel }
                             ]
                 , className: "carousel"
                 }
          else empty

emptyCarousel :: forall a. Monoid a => Carousel a
emptyCarousel = (Carousel Nil mempty 0 0 Nil)

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel String } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ c _ _ _) = carousel
                pure $ R.div { children: [ R.img { src: c
                                                 , className: "carousel-img"
                                                 }
                                         ]
                             , className: "carousel-img-container"
                             }

mkCarouselControl :: CarouselAction
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

mkCarouselCounter :: CarouselAction
mkCarouselCounter = component "CarouselCounter" \{ carousel } -> React.do
                    let (Carousel _ _ curr total _ ) = carousel
                    pure $ R.text $ show curr <> " / " <> show total

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
