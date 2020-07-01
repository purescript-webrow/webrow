module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WebRow.HTTP (spec) as HTTP

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  HTTP.spec
