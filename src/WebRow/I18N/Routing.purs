module WebRow.I18N.Routing where

import Prelude hiding ((/))

import Control.Alt ((<|>))
import Data.Either (note)
import Data.Newtype (un)
import Data.Variant (class Contractable, Variant)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', as, segment)
import Routing.Duplex (print) as D
import Run (Run)
import Run.Reader (askAt)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept)
import WebRow.HTTP (redirect) as HTTP.Response
import WebRow.I18N.ISO639.TwoLetter (Languages, languageCode, parse, toString)
import WebRow.Routing (FullUrl(..), ROUTING, RelativeUrl(..), _routing, fromRelativeUrl)
import WebRow.Routing (Routing, printFullRoute, printRoute) as Routing

type Route' langs route = { language ∷ Variant langs, route ∷ route }

duplex
  ∷ ∀ langs route
  . Contractable Languages langs
  ⇒ Variant langs
  → RouteDuplex' route
  → RouteDuplex' (Route' langs route)
duplex default (RouteDuplex routePrinter routeParser) =
  RouteDuplex printer parser
  where
    RouteDuplex langPrinter langParser =
      (as (languageCode >>> toString) (parse >>> note "Invalid language code")) segment

    printer { language: l, route: r } = if languageCode l == languageCode default
      then routePrinter r
      else langPrinter l <> routePrinter r

    parser
      = ({ language:_, route: _ } <$> langParser <*> routeParser)
      <|> ({ language: default, route: _ } <$> routeParser)

type Routing' langs routes eff = Routing.Routing (Route' langs routes) eff

type ROUTING' (langs ∷ # Type) route = ROUTING (Route' langs route)

printRoute ∷ ∀ eff langs route. route → Run ( routing ∷ ROUTING' langs route | eff ) RelativeUrl
printRoute v = map RelativeUrl $ do
  routing ← askAt _routing
  pure $ D.print routing.routeDuplex { language: routing.route.language, route: v }

printFullRoute ∷ ∀ eff langs route. route → Run ( routing ∷ ROUTING' langs route | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (askAt _routing <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

translatedRoute ∷ ∀ eff langs route. Variant langs → route → Run (routing ∷ ROUTING' langs route | eff ) RelativeUrl
translatedRoute lang v = Routing.printRoute { language: lang, route: v }

translatedFullRoute ∷ ∀ eff langs route. Variant langs → route → Run (routing ∷ ROUTING' langs route | eff ) FullUrl
translatedFullRoute lang v = Routing.printFullRoute { language: lang, route: v }

fullRoute ∷ ∀ eff langs route. Run (Routing' langs route + eff) (Route' langs route)
fullRoute = _.route <$> askAt _routing

route ∷ ∀ eff langs route. Run (Routing' langs route + eff) route
route =  _.route <<< _.route <$> askAt _routing

language ∷ ∀ eff langs route. Run (Routing' langs route + eff) (Variant langs)
language = _.language <<< _.route <$> askAt _routing

redirect
  ∷ ∀ a eff langs route
  . route
  → Run
      ( HTTPExcept
      + Routing' langs route
      + eff
      )
      a
redirect r = do
  url ← printRoute r
  HTTP.Response.redirect (fromRelativeUrl url)

