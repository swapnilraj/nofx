module Components.Editor where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler, handler_, merge)
import React.Basic.Events as Events
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue, timeStamp)
import React.Basic.Hooks (ReactComponent, Hook, UseState, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

mkEditor :: Effect (ReactComponent
                      { setProg :: (String -> String) -> Effect Unit
                      }
                   )
mkEditor = component "Editor" \{ setProg } -> React.do
           editor <- useInput ""
           pure $ R.div_ [ R.textarea { onChange: editor.onChange
                                      , value: editor.value
                                      }
                          , R.button { onClick: handler_ $ setProg $ const editor.value
                                     , children: [ R.text "Run" ]
                                     }
                          ]

useInput ::
  String ->
  Hook
    (UseState { value :: String, lastChanged :: Maybe Number })
    { onChange :: EventHandler
    , value :: String
    , lastChanged :: Maybe Number
    }
useInput initialValue = React.do
  { value, lastChanged } /\ replaceState <- useState { value: initialValue, lastChanged: Nothing }
  pure
    { onChange:
      handler
        (preventDefault >>> stopPropagation >>> merge { targetValue, timeStamp }) \{ timeStamp, targetValue } -> do
        replaceState \_ ->
          { value: fromMaybe "" targetValue
          , lastChanged: Just timeStamp
          }
    , value
    , lastChanged
    }
