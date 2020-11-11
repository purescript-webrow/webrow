module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WebRow.Applets (spec) as Applets
import Test.WebRow.HTTP (spec) as HTTP
import Test.WebRow.I18N (spec) as I18N
import Test.WebRow.PostgreSQL.Config (load) as Test.PostgreSQL.Config
import Test.WebRow.PostgreSQL.PG (spec) as PG
import Test.WebRow.Selda (spec) as Selda
import Test.WebRow.Session (spec) as Session

main :: Effect Unit
main = launchAff_ $ do

  -- | I have to extract common Aff actions here
  -- | like pg pool construction so I don't repeat
  -- | it on every test entry.
  pool ← Test.PostgreSQL.Config.load
  runSpec [consoleReporter] do
    I18N.spec
    Applets.spec
    HTTP.spec
    Session.spec

    PG.spec pool
    Selda.spec pool
