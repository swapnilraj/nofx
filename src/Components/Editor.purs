module Components.Editor where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Events (handler, handler_)
import React.Basic.Events as Events
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkEditor :: Effect (ReactComponent { setState :: (Boolean -> Boolean) -> Effect
Unit} )
mkEditor = component "Editor" \{ setState } -> React.do
  pure $ R.div_ [ R.textarea {}
                , R.button { onClick: handler_ $ setState $ const false
                           , children: [ R.text "Run" ]
                           }
                ]
