module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Either
import Text.Parsing.Parser

import Data.List (last)
import Data.Maybe

import Parser
import Compiler
import Lift
import Desugar
import Interpreter

import Partial.Unsafe

{-- main :: Effect Unit --}
{-- main = do --}
{--   logShow $ doMyThing "fun main = fib 4\n\nfun fib x = if eq x 1 then 1 else (let rec y = sub x 1;t = fib y; i = mul 2 t in i)" --}

main :: Effect Unit
main = do
  logShow $
    doMyThing
    "fun main = fac 4\nfun fac n = if eq n 1 then 1 else mul n (fac (sub n 1))"

doMyThing s =
  let t = unsafePartial $ ( interpret <<< compile <<< lambdaLift <<< desugarSCs <<< fromRight) $ runParser s supercombinators
      y = case last t of
              Just a -> a
              Nothing -> unsafeCrashWith "OOO"
  in y.output

doTT s = unsafePartial $ ( compile <<< lambdaLift <<< desugarSCs <<< fromRight) $ runParser s supercombinators

