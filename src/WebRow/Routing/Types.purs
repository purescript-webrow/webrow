module WebRow.Routing.Types where

import Data.Lazy (Lazy)
import Data.Newtype (class Newtype)
import Polyform.Batteries.UrlEncoded (Query)
import Routing.Duplex (RouteDuplex') as D
import Routing.Duplex.Types (RouteState) as Routing.Duplex.Types

newtype FullUrl
  = FullUrl String

derive instance newtypeFullUrl ∷ Newtype FullUrl _

newtype RelativeUrl
  = RelativeUrl String

derive instance newtypeRelativeUrl ∷ Newtype RelativeUrl _

newtype Url
  = Url String

derive instance newtypeUrl ∷ Newtype Url _

fromFullUrl ∷ FullUrl → Url
fromFullUrl (FullUrl url) = Url url

fromRelativeUrl ∷ RelativeUrl → Url
fromRelativeUrl (RelativeUrl url) = Url url

type Domain
  = String

type Context v
  = { domain ∷ Domain
    , routeDuplex ∷ D.RouteDuplex' v
    , raw ∷ Routing.Duplex.Types.RouteState
    , route ∷ v
    , query ∷ Lazy Query
    , url ∷ RelativeUrl
    }
