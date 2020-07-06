module WebRow.Cookies.CookieStore where

import Prelude

import Data.Array.NonEmpty (head, singleton) as Array.NonEmpty
import Data.Either (hush)
import Data.Lazy (Lazy, defer)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Data.Map (insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object as Object
import HTTPure as HTTPure
import Node.Simple.Jwt (Secret)
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Crypto.String (sign, unsign) as Crypto.String
import WebRow.HTTP.Cookies.Headers (clientCookies)
import WebRow.HTTP.Cookies.Types (ClientCookies, Name, SetValue, Value, Values)

newtype CookieStore = CookieStore
  { clientCookies ∷ Lazy ClientCookies
  , setCookies ∷ Map
      Name
      SetValue
  }

cookieStore ∷ HTTPure.Request → CookieStore
cookieStore req = CookieStore
  { clientCookies: defer \_ → clientCookies req.headers
  , setCookies: mempty
  }

lookup ∷ Secret → Name → CookieStore → Lazy (Maybe Value)
lookup secret name = map (map Array.NonEmpty.head) <<< lookup' secret name

lookup' ∷ Secret → Name → CookieStore → Lazy (Maybe Values)
lookup' secret name (CookieStore { clientCookies, setCookies }) =
  defer \_ → signed >>= traverse unsign
  where
    unsign v = hush (Crypto.String.unsign secret v)
    signed = case name `Map.lookup` setCookies of
      Just { value, attributes } | attributes.expires /= Just epoch → Just (Array.NonEmpty.singleton value)
      Just _ → Nothing
      Nothing → name `Object.lookup` (Lazy.force clientCookies)

set ∷ Secret → Name → SetValue → CookieStore → Maybe CookieStore
set secret name { value, attributes } (CookieStore { clientCookies, setCookies }) = ado
  value' ← hush (Crypto.String.sign secret value)
  in CookieStore
    { clientCookies
    , setCookies: Map.insert name { attributes, value: value' } setCookies
    }

