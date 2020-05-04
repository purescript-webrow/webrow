module WebRow.Route where

import Prelude

import Data.Variant (SProxy(..), Variant)
import Routing.Duplex as D
import Run (FProxy, Run)
import Run as Run

type RouteInfo v =
  { domain ∷ String
  , route ∷ D.RouteDuplex' (Variant v)
  }

data RouteF v a
  = HasRoute ( RouteInfo v → a )

derive instance functorRouteF ∷ Functor (RouteF v)

type ROUTE v = FProxy (RouteF v)

_route = SProxy ∷ SProxy "route"

hasRoutes
  ∷ ∀ eff v
  . Run ( route ∷ ROUTE v | eff ) (RouteInfo v)
hasRoutes = Run.lift _route (HasRoute identity)

printRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) String
printRoute v = hasRoutes <#> _.route <#> flip D.print v 

printFullRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) String
printFullRoute v = (<>) <$> (hasRoutes <#> _.domain) <*> printRoute v

interpretRoute
  ∷ ∀ a v eff
  . RouteInfo v
  → Run ( route ∷ FProxy (RouteF v) | eff ) a
  → Run eff a
interpretRoute info = Run.interpret
  (Run.on _route (\(HasRoute k) → pure $ k info) Run.send)
