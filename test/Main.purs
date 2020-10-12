module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WebRow.Applets (spec) as Applets
import Test.WebRow.HTTP (spec) as HTTP
import Test.WebRow.I18N (spec) as I18N

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  I18N.spec
  Applets.spec
  HTTP.spec
