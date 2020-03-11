module Components.App where

import Prelude

import Components.Editor (mkEditor)
import Components.Viz (mkViz)

import Effect (Effect)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = do
  editor <- mkEditor
  viz <- mkViz
  component "App" \props -> React.do
    prog /\ setProg <- useState ""
    pure $ R.div { children: [ element editor { setProg }
                             , element viz { prog }
                             ]
                }
