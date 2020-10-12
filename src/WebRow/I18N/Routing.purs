module WebRow.I18N.Routing where

import Prelude hiding ((/))

import Control.Alt ((<|>))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Variant (class Contractable, Variant)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, segment)
import WebRow.I18N.ISO639.TwoLetter (LanguageCode, Languages, code, parse, toString)

data I18NRoute lang route
  = Translated lang route
  | Untranslated route
derive instance eqI18NRoute ∷ (Eq lang, Eq route) ⇒ Eq (I18NRoute lang route)
derive instance genericI18NRoute ∷ Generic (I18NRoute lang route) _
instance showI18NRoute ∷ (Show lang, Show route) ⇒ Show (I18NRoute lang route) where
  show s = genericShow s

duplex
  ∷ ∀ langs route
  . Contractable Languages langs
  ⇒ RouteDuplex' route
  → RouteDuplex' (I18NRoute (Variant langs) route)
duplex (RouteDuplex routePrinter routeParser) =
  RouteDuplex printer parser
  where
    RouteDuplex langPrinter langParser =
      (as (code >>> toString) (parse >>> note "Invalid language code")) segment

    printer (Translated l route) =
      langPrinter l <> routePrinter route
    printer (Untranslated route) =
      routePrinter route

    parser
      = (Translated <$> langParser <*> routeParser)
      <|> (Untranslated <$> routeParser)

routeCode ∷ ∀ lang route. Contractable Languages lang ⇒ I18NRoute (Variant lang) route → Maybe LanguageCode
routeCode (Translated lang _) = Just (code lang)
routeCode (Untranslated _) = Nothing
