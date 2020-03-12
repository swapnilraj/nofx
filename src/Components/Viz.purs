module Components.Viz where

import Prelude

import Data.List (List)
import Data.Traversable (sequence)

import Compiler (GmState)
import Graphics (renderGmState)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

mkViz :: Effect (ReactComponent { prog :: List GmState } )
mkViz = component "Viz" \{ prog } -> React.do
          rendered <- useAff prog (sequence $ renderGmState <$> prog)
          pure $ R.div_ [ R.text (show rendered) ]
