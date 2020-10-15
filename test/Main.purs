module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WebRow.Applets (spec) as Applets
import Test.WebRow.HTTP (spec) as HTTP
import Test.WebRow.I18N (spec) as I18N
import Test.WebRow.Selda (spec) as Selda
import Test.WebRow.Selda.Config (load) as Test.Selda.Config
import Test.WebRow.Session (spec) as Session

main :: Effect Unit
main = launchAff_ $ do

  -- | I have to extract common Aff action here
  -- | so I don't repeat it on every test entry
  pool ← Test.Selda.Config.load
  runSpec [consoleReporter] do
    I18N.spec
    Applets.spec
    HTTP.spec
    Session.spec

    Selda.spec pool
