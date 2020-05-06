module WebRow.Route where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Variant (SProxy(..), Variant)
import Routing.Duplex as D
import Run (FProxy, Run)
import Run as Run

newtype FullUrl = FullUrl String
derive instance newtypeFullUrl ∷ Newtype FullUrl _

newtype RelativeUrl = RelativeUrl String
derive instance newtypeRelativeUrl ∷ Newtype RelativeUrl _

newtype Url = Url String
derive instance newtypeUrl ∷ Newtype Url _

fromFullUrl :: FullUrl -> Url
fromFullUrl (FullUrl url) = Url url

fromRelativeUrl :: RelativeUrl -> Url
fromRelativeUrl (RelativeUrl url) = Url url

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

printRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) RelativeUrl
printRoute v = map RelativeUrl $ hasRoutes <#> _.route <#> flip D.print v

printFullRoute ∷ ∀ v eff. Variant v → Run ( route ∷ ROUTE v | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (hasRoutes <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

interpret
  ∷ ∀ a v eff
  . RouteInfo v
  → Run ( route ∷ FProxy (RouteF v) | eff ) a
  → Run eff a
interpret info = Run.interpret
  (Run.on _route (\(HasRoute k) → pure $ k info) Run.send)
