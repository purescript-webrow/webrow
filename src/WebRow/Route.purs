module WebRow.Route where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Variant (SProxy(..), Variant)
import Routing.Duplex as D
import Run (Run)
import Run.Reader (READER, askAt)

newtype FullUrl = FullUrl String
derive instance newtypeFullUrl ∷ Newtype FullUrl _

newtype RelativeUrl = RelativeUrl String
derive instance newtypeRelativeUrl ∷ Newtype RelativeUrl _

newtype Url = Url String
derive instance newtypeUrl ∷ Newtype Url _

fromFullUrl ∷ FullUrl → Url
fromFullUrl (FullUrl url) = Url url

fromRelativeUrl ∷ RelativeUrl → Url
fromRelativeUrl (RelativeUrl url) = Url url

type RoutingCtx v =
  { domain ∷ String
  , routeDuplex ∷ D.RouteDuplex' (Variant v)
  }

_route = SProxy ∷ SProxy "route"

type ROUTE route = READER (RoutingCtx route)

type Route route eff = (route ∷ ROUTE route | eff)

printRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) RelativeUrl
printRoute v = map RelativeUrl $ askAt _route <#> _.routeDuplex <#> flip D.print v

printFullRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (askAt _route <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

