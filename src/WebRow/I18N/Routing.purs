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
import WebRow.I18N.ISO639.TwoLetter (Languages, code, parse, toString)
import WebRow.Routing (FullUrl(..), ROUTING, RelativeUrl(..), Routing, _routing, fromRelativeUrl)
import WebRow.Routing (printFullRoute, printRoute) as Routing

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

    printer { language, route: r } = if code language == code default
      then routePrinter r
      else langPrinter language <> routePrinter r

    parser
      = ({ language:_, route: _ } <$> langParser <*> routeParser)
      <|> ({ language: default, route: _ } <$> routeParser)

type Routing' langs routes eff = Routing ({ language ∷ Variant langs, route ∷ Variant routes }) eff

type ROUTING' (langs ∷ # Type) (v ∷ # Type) = ROUTING ({ language ∷ Variant langs, route ∷ Variant v })

printRoute ∷ ∀ langs v eff. Variant v → Run ( routing ∷ ROUTING' langs v | eff ) RelativeUrl
printRoute v = map RelativeUrl $ do
  routing ← askAt _routing
  pure $ D.print routing.routeDuplex { language: routing.route.language, route: v }

printFullRoute ∷ ∀ eff langs v. Variant v → Run ( routing ∷ ROUTING' langs v | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (askAt _routing <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

translatedRoute ∷ ∀ langs v eff. Variant langs → Variant v → Run (routing ∷ ROUTING' langs v | eff ) RelativeUrl
translatedRoute lang v = Routing.printRoute { language: lang, route: v }

translatedFullRoute ∷ ∀ langs v eff. Variant langs → Variant v → Run (routing ∷ ROUTING' langs v | eff ) FullUrl
translatedFullRoute lang v = Routing.printFullRoute { language: lang, route: v }

route ∷ ∀ eff langs route. Run (Routing' langs route + eff) (Variant route)
route =  _.route <<< _.route <$> askAt _routing

redirect
  ∷ ∀ a eff langs route
  . Variant route
  → Run
      ( HTTPExcept
      + Routing' langs route
      + eff
      )
      a
redirect r = do
  url ← printRoute r
  HTTP.Response.redirect (fromRelativeUrl url)

