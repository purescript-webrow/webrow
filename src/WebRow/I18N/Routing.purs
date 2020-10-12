module WebRow.I18N.Routing where

import Prelude hiding ((/))

import Control.Alt ((<|>))
import Data.Either (note)
import Data.Variant (class Contractable, Variant)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, segment)
import WebRow.I18N.ISO639.TwoLetter (Languages, code, parse, toString)

duplex
  ∷ ∀ langs route
  . Contractable Languages langs
  ⇒ Variant langs
  → RouteDuplex' route
  → RouteDuplex' { language ∷ Variant langs, route ∷ route }
duplex default (RouteDuplex routePrinter routeParser) =
  RouteDuplex printer parser
  where
    RouteDuplex langPrinter langParser =
      (as (code >>> toString) (parse >>> note "Invalid language code")) segment

    printer { language, route } = if code language == code default
      then routePrinter route
      else langPrinter language <> routePrinter route

    parser
      = ({ language:_, route: _ } <$> langParser <*> routeParser)
      <|> ({ language: default, route: _ } <$> routeParser)

