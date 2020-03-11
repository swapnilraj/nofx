module Main where

import Prelude

import Components.App (mkApp)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.Hooks(element)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Partial.Unsafe

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  appContainer <- mkApp
  let app = element appContainer {}
  case root of
    Nothing -> throw "Root element not found."
    Just r  -> render app r
