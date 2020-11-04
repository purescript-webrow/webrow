module Test.WebRow.I18N where

import Prelude hiding ((/))
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Routing.Duplex (RouteDuplex', parse, print)
import Routing.Duplex (int, root, segment) as Duplex
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic (sum) as Duplex
import Routing.Duplex.Generic.Syntax ((/))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.I18N (languageCode, getLanguage)
import WebRow.I18N.ISO639.TwoLetter (LanguageNames)
import WebRow.I18N.Routing (duplex) as I18N.Routing

type Latine r
  = ( la ∷ LanguageNames | r )

type Nepali r
  = ( ne ∷ LanguageNames | r )

type Urdu r
  = ( ur ∷ LanguageNames | r )

type Language
  = Variant (Latine + Nepali + Urdu ())

la ∷ Language
la = getLanguage (SProxy ∷ SProxy "la")

ne ∷ Language
ne = getLanguage (SProxy ∷ SProxy "ne")

ur ∷ Language
ur = getLanguage (SProxy ∷ SProxy "ur")

type Id
  = Int

data ActualRoute
  = Home
  | Profile Id

derive instance genericActualRoute ∷ Generic ActualRoute _

derive instance eqActualRoute ∷ Eq ActualRoute

derive instance ordActualRoute ∷ Ord ActualRoute

instance showActualRoute ∷ Show ActualRoute where
  show = genericShow

actualRouteDuplex :: RouteDuplex' ActualRoute
actualRouteDuplex =
  Duplex.sum
    { "Home": noArgs
    , "Profile": "profile" / Duplex.int Duplex.segment
    }

duplex ∷ RouteDuplex' { language ∷ Language, route ∷ ActualRoute }
duplex = Duplex.root $ I18N.Routing.duplex la actualRouteDuplex

spec ∷ Spec Unit
spec = do
  describe "I18N" do
    describe "route duplex" do
      it "should print translated path" do
        let
          path = print duplex ({ language: ur, route: Home })
        shouldEqual "/ur" path
      it "should print empty prefix for default lang" do
        let
          path = print duplex ({ language: la, route: Home })
        shouldEqual "/" path
      it "should parse translated path" do
        let
          lang = parse duplex ("/ur/profile/8")
        shouldEqual (hush lang <#> _.language >>> languageCode) (Just (languageCode ur))
        shouldEqual (hush lang <#> _.route) (Just (Profile 8))
      it "should parse default langauge path" do
        let
          lang = parse duplex ("/")
        shouldEqual (hush lang <#> _.language >>> languageCode) (Just (languageCode la))
