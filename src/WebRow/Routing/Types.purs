module WebRow.Routing.Types where

import Data.Lazy (Lazy)
import Data.Newtype (class Newtype)
import Polyform.Batteries.UrlEncoded (Decoded)
import Routing.Duplex (RouteDuplex') as D

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
  , routeDuplex ∷ D.RouteDuplex' v
  , route ∷ v
  , query ∷ Lazy Decoded
  }

