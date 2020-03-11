module Components.App where

import Prelude

import Data.List (List(..))

import Compiler (GmState)

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
    prog /\ setProg <- useState ( Nil :: List GmState)
    pure $ R.div_ [ element editor { setProg }
                  , element viz { prog }
                  ]
