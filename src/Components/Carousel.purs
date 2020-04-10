module Components.Carousel where

import Prelude

import Data.List (List(..), head, init, last, length, singleton, tail)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Monoid.Additive
import Data.Monoid.Disj

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, empty)

import Compiler (GmState, emptyGmState)

data Carousel a = Carousel (List a) a (List a)

toCarousel :: List (GmState /\ String) -> Carousel (GmState /\ String)
toCarousel l = Carousel Nil hd tl
  where hd = fromMaybe (emptyGmState /\ "") $ head l
        tl = fromMaybe Nil $ tail l

nextCarousel :: Carousel (GmState /\ String) -> Carousel (GmState /\ String)
nextCarousel c@(Carousel _ _ Nil) = c
nextCarousel (Carousel bef curr after) =
  Carousel (bef <> singleton curr) curr' tl
  where curr' = fromMaybe (emptyGmState /\ "") $ head after
        tl = fromMaybe Nil $ tail after

previousCarousel :: Carousel (GmState /\ String) -> Carousel (GmState /\ String)
previousCarousel c@(Carousel Nil _ _) = c
previousCarousel (Carousel bef curr after) =
  Carousel ini lt (Cons curr after)
  where ini = fromMaybe Nil $ init bef
        lt = fromMaybe (emptyGmState /\ "") $ last bef

type CarouselAction a = Effect (ReactComponent
                        { carousel :: Carousel a
                        , setCarousel :: (Carousel a -> Carousel a) -> Effect Unit
                        })

mkCarousel :: CarouselAction (GmState /\ String)
mkCarousel = do
  carouselCode <- mkCarouselCode
  carouselImg <- mkCarouselImg
  carouselControl <- mkCarouselControl
  component "Carousel" \{ carousel, setCarousel } -> React.do
    let (Carousel _ curr _ ) = carousel
    pure $ if curr /= (emptyGmState /\ "")
          then R.div { children: [ element carouselImg { carousel }
                                 , element carouselCode { carousel }
                                 , element carouselControl { carousel, setCarousel }
                                 ]
                    , className: "carousel"
                    }
          else empty

emptyCarousel :: Carousel (GmState /\ String)
emptyCarousel = Carousel Nil (emptyGmState /\ "") Nil

mkCarouselCode :: Effect (ReactComponent { carousel :: Carousel (GmState /\ String) } )
mkCarouselCode = component "CarouselCode" \{ carousel } -> React.do
                 let (Carousel bef _ _) = carousel
                 let instructions = maybe Nil (fst >>> _.code) $ last bef
                 let curr = maybe "" show $ head instructions
                 pure $ R.h3_ [ R.text curr ]

mkCarouselImg :: Effect (ReactComponent { carousel :: Carousel (GmState /\ String) } )
mkCarouselImg = component "CarouselImg" \{ carousel } -> React.do
                let (Carousel _ (_ /\ c) _) = carousel
                pure $ R.div { children: [ R.img { src: c
                                                 , className: "carousel-img"
                                                 }
                                         ]
                             , className: "carousel-img-container"
                             }

mkCarouselControl :: CarouselAction (GmState /\ String)
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

mkCarouselCounter :: CarouselAction (GmState /\ String)
mkCarouselCounter = component "CarouselCounter" \{ carousel } -> React.do
                    let (Carousel before ( { stats } /\ _) after ) = carousel
                        total = length before + length after
                    pure $ R.text $ show stats <> " / " <> show total

mkPreviousButton :: CarouselAction (GmState /\ String)
mkPreviousButton = component "PreviousButton" \{ carousel, setCarousel } -> React.do
                let previous = previousCarousel carousel
                pure $ R.button { children: [ R.text "Previous" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextButton :: CarouselAction (GmState /\ String)
mkNextButton = component "NextButton" \{ carousel, setCarousel } -> React.do
                let next = nextCarousel carousel
                pure $ R.button { children: [ R.text "Next" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }

mkPreviousUnwind :: CarouselAction (GmState /\ String)
mkPreviousUnwind = component "PreviousUnwindButton" \{ carousel, setCarousel } -> React.do
                let previous = findPreviousUnwind carousel
                pure $ R.button { children: [ R.text "Previous Reduction" ]
                                , onClick: handler_ $ setCarousel $ const previous
                                }

mkNextUnwind :: CarouselAction (GmState /\ String)
mkNextUnwind = component "NextUnwindButton" \{ carousel, setCarousel } -> React.do
                let next = findNextUnwind carousel
                pure $ R.button { children: [ R.text "Next Reduction" ]
                                , onClick: handler_ $ setCarousel $ const next
                                }

findNextUnwind :: Carousel (GmState /\ String) ->
                  Carousel (GmState /\ String)
findNextUnwind = go <<< nextCarousel
  where
  go n@(Carousel _ _ Nil) = n
  go n@(Carousel _ ({ isUnwind: true } /\ _) _) = n
  go n = go $ nextCarousel n

findPreviousUnwind :: Carousel (GmState /\ String) ->
                      Carousel (GmState /\ String)
findPreviousUnwind = go <<< previousCarousel
  where
  go n@(Carousel Nil _ _) = n
  go n@(Carousel _ ({ isUnwind: true } /\ _) _) = n
  go n = go $ previousCarousel n
