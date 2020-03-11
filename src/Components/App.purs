module Components.App where

import Prelude

import Effect (Effect)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = component "App" \props -> React.do
  pure $ R.div { children: [ R.text "NoFx" ] }
