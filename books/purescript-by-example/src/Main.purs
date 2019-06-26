module Main where

import Prelude
import Chapter2 (diagonal)

import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = logShow (diagonal 3.0 4.0)
