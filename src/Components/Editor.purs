module Components.Editor where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)

import Compiler (GmState, compile)
import Interpreter (interpret)
import Lift (lambdaLift)
import Desugar (desugarSCs)
import Parser (supercombinators)
import Text.Parsing.Parser (runParser)
import Data.Either (fromRight)

import Partial.Unsafe (unsafePartial)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler, handler_, merge)
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue, timeStamp)
import React.Basic.Hooks (ReactComponent, Hook, UseState, component, useState, (/\))
import React.Basic.Hooks as React

mkEditor :: Effect (ReactComponent
                      { setProg :: (List GmState -> List GmState) -> Effect Unit
                      }
                   )
mkEditor = component "Editor" \{ setProg } -> React.do
           editor <- useInput ""
           pure $ R.div
                    { children: [ R.button { onClick: handler_ $
                                                setProg $
                                                \_ ->
                                                toGmState editor.value
                                           , children: [ R.div { className: "arrow-right" }
                                                       , R.text "Run"
                                                       ]
                                           , className: "editor-run"
                                           }
                                , R.textarea { onChange: editor.onChange
                                             , value: editor.value
                                             , className: "editor-textarea"
                                             }
                                ]
                    , className: "editor"
                    }
  where
    toGmState s = unsafePartial $
                  ( interpret <<<
                    compile <<<
                    lambdaLift <<<
                    desugarSCs <<<
                    fromRight) $
                    runParser s supercombinators


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
