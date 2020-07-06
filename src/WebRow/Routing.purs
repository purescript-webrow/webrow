module WebRow.Routing where

import Prelude

import Data.Array (singleton) as Array
import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.Lazy (defer) as L
import Data.Map (fromFoldableWith) as Map
import Data.Newtype (class Newtype, un)
import Data.String (Pattern(..), Replacement(..), replaceAll) as String
import Data.Variant (SProxy(..), Variant)
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Routing.Duplex (RouteDuplex', print) as D
import Routing.Duplex (RouteDuplex(..))
import Routing.Duplex.Parser (RouteError, RouteResult(..), parsePath, runRouteParser) as D
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

type Domain = String

type Context v =
  { domain ∷ Domain
  , routeDuplex ∷ D.RouteDuplex' (Variant v)
  , route ∷ Variant v
  , query ∷ Lazy Decoded
  }

_routing = SProxy ∷ SProxy "routing"

type ROUTING route = READER (Context route)

type Routing route eff = (routing ∷ ROUTING route | eff)

printRoute ∷ ∀ v eff. Variant v → Run ( routing ∷ ROUTING v | eff ) RelativeUrl
printRoute v = map RelativeUrl $ askAt _routing <#> _.routeDuplex <#> flip D.print v

printFullRoute ∷ ∀ v eff. Variant v → Run ( routing ∷ ROUTING v | eff ) FullUrl
printFullRoute v = map FullUrl $ (<>) <$> (askAt _routing <#> _.domain) <*> (map (un RelativeUrl) $ printRoute v)

context ∷ ∀ v. Domain → D.RouteDuplex' (Variant v) → String → Either D.RouteError (Context v)
context domain routeDuplex@(RouteDuplex _ dec) = go
  where
    replacePlus = String.replaceAll (String.Pattern "+") (String.Replacement " ")
    go url =
      let
        routeState@{ params } = D.parsePath (replacePlus url)
        query = L.defer \_ →
          Decoded
          <<< Map.fromFoldableWith append
          <<< map (map Array.singleton)
          $ params
        ctx =
          { route: _
          , query
          , routeDuplex
          , domain
          }
      in
        ctx <$> case D.runRouteParser routeState dec of
          D.Fail err → Left err
          D.Success _ res → Right res
