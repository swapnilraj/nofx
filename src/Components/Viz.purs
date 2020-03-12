module Components.Viz where

import Prelude

import Data.Array (fromFoldable)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

import Global (encodeURIComponent)

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = component "Viz" \{ prog } -> React.do
          rendered <- useAff prog (sequence $ renderGmState <$> prog)
          case rendered of
               Just (Right imgs) -> pure $ RB.fragment $ fromFoldable $ vizImg <$> imgs
               _ -> pure $ R.div_ [ R.text "Empty" ]

vizImg :: String -> React.JSX
vizImg imgData = R.img { src: "data:image/svg+xml;charset=utf8," <>
                         (fromMaybe "" $ encodeURIComponent imgData)
                       }
