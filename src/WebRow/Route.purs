module WebRow.Route where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Variant (SProxy(..), Variant)
import Routing.Duplex as D
import Run (FProxy, Run)
import Run as Run
import Type.Row (type (+))

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
  , route ∷ D.RouteDuplex' (Variant v)
  }

data RouteF v a
  = RoutingCtx ( RoutingCtx v → a )

derive instance functorRouteF ∷ Functor (RouteF v)

type ROUTE v = FProxy (RouteF v)

_route = SProxy ∷ SProxy "route"

type Route routes eff = (route ∷ ROUTE routes | eff)

routingCtx
  ∷ ∀ eff v
  . Run ( route ∷ ROUTE v | eff ) (RoutingCtx v)
routingCtx = Run.lift _route (RoutingCtx identity)

printRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) RelativeUrl
printRoute v = map RelativeUrl $ routingCtx <#> _.route <#> flip D.print v

printFullRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (routingCtx <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

interpret
  ∷ ∀ a v eff
  . RoutingCtx v
  → Run ( Route v + eff ) a
  → Run eff a
interpret info = Run.interpret
  (Run.on _route (\(RoutingCtx k) → pure $ k info) Run.send)
