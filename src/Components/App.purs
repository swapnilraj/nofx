module Components.App where

import Prelude

import Components.Editor (mkEditor)

import Effect (Effect)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = do
  editor <- mkEditor
  component "App" \props -> React.do
    on /\ setState <- useState true
    pure $ R.div { children: [ element editor { setState }
                             , R.text $ if on then "T" else "F"
                             ]
                }
