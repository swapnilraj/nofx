module Components.Carousel where

import Prelude

import Data.List (List(..), head, init, last, length, singleton, tail)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Monoid.Additive
import Data.Monoid.Disj

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

mkCarousel :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
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
emptyCarousel = Carousel Nil mempty Nil

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel (Disj Boolean /\ Additive Int /\ String) } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ (_ /\ _ /\ c) _) = carousel
                pure $ R.div { children: [ R.img { src: c
                                                 , className: "carousel-img"
                                                 }
                                         ]
                             , className: "carousel-img-container"
                             }

mkCarouselControl :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkCarouselControl = do
  carouselConuter <- mkCarouselCounter
  nextButton <- mkNextButton
  previousButton <- mkPreviousButton
  previousUnwind <- mkPreviousUnwind
  nextUnwind <- mkNextUnwind
  component "CarouselControl" \props -> React.do
     pure $ R.div { children: [ element carouselConuter props
                              , R.div { children: [ element previousUnwind props
                                                  , element previousButton props
                                                  , element nextButton props
                                                  , element nextUnwind props
                                                  ]
                                      , className: "carousel-control"
                                      }
                              ]
                  , className: "carousel-info"
                  }

mkCarouselCounter :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkCarouselCounter = component "CarouselCounter" \{ carousel } -> React.do
                    let (Carousel before (_ /\ Additive curr /\ _) after ) = carousel
                        total = length before + length after
                    pure $ R.text $ show curr <> " / " <> show total

mkPreviousButton :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkPreviousButton = component "PreviousButton" \{ carousel, setCarousel } -> React.do
                let previous = previousCarousel carousel
                pure $ R.button { children: [ R.text "Previous" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextButton :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkNextButton = component "NextButton" \{ carousel, setCarousel } -> React.do
                let next = nextCarousel carousel
                pure $ R.button { children: [ R.text "Next" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }

mkPreviousUnwind :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkPreviousUnwind = component "PreviousUnwindButton" \{ carousel, setCarousel } -> React.do
                let previous = findPreviousUnwind carousel
                pure $ R.button { children: [ R.text "Previous Unwind" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextUnwind :: CarouselAction (Disj Boolean /\ Additive Int /\ String)
mkNextUnwind = component "NextUnwindButton" \{ carousel, setCarousel } -> React.do
                let next = findNextUnwind carousel
                pure $ R.button { children: [ R.text "Next Unwind" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }

findNextUnwind :: Carousel (Disj Boolean /\ Additive Int /\ String) ->
                  Carousel (Disj Boolean /\ Additive Int /\ String)
findNextUnwind = go <<< nextCarousel
  where
  go n@(Carousel _ _ Nil) = n
  go n@(Carousel _ (Disj true /\ _ /\ _) _) = n
  go n = go $ nextCarousel n

findPreviousUnwind :: Carousel (Disj Boolean /\ Additive Int /\ String) ->
                  Carousel (Disj Boolean /\ Additive Int /\ String)
findPreviousUnwind = go <<< previousCarousel
  where
  go n@(Carousel Nil _ _) = n
  go n@(Carousel _ (Disj true /\ _ /\ _) _) = n
  go n = go $ previousCarousel n
