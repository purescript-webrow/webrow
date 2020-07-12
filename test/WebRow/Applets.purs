module Test.WebRow.Applets where

import Prelude

import Test.Spec (Spec, describe)
import Test.WebRow.Applets.Auth (spec) as Test.Webrow.Applets.Auth
import Test.WebRow.Applets.Registration (spec) as Test.Webrow.Applets.Registration

spec ∷ Spec Unit
spec = do
  describe "WebRow.Applets" do
    Test.Webrow.Applets.Auth.spec
    Test.Webrow.Applets.Registration.spec
