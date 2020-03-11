module Components.Viz where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Events (handler, handler_)
import React.Basic.Events as Events
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkViz :: Effect (ReactComponent { prog :: String } )
mkViz = component "Viz" \{ prog } -> React.do
        pure $ R.div_ [ R.img { src: prog } ]
