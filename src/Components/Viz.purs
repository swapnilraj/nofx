module Components.Viz where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), hush)
import Data.Traversable (sequence)

import Components.Carousel (mkCarousel, toCarousel, emptyCarousel)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, empty, element, useState, useEffect, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

import Global (encodeURIComponent)

mkImgSrc :: String -> String
mkImgSrc imgData =
  "data:image/svg+xml;charset=utf8," <>
  (fromMaybe "" $ encodeURIComponent imgData)

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = do
  carouselEl <- mkCarousel
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
          pure $ R.div_ [ element carouselEl { carousel, setCarousel } ]
        _ -> pure $ empty
