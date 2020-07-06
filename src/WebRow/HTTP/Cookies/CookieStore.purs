module WebRow.Cookies.CookieStore where

import Prelude

import Data.Array.NonEmpty (head, singleton) as Array.NonEmpty
import Data.Either (hush)
import Data.Lazy (Lazy, defer)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Data.Map (insert, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import HTTPure as HTTPure
import Node.Simple.Jwt (Secret)
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Crypto.String (sign, unsign) as Crypto.String
import WebRow.HTTP.Cookies.Headers (clientCookies, setCookieHeader)
import WebRow.HTTP.Cookies.Types (ClientCookies, Name, SetValue, Value, Values)

newtype CookieStore = CookieStore
  { clientCookies ∷ Lazy ClientCookies
  , secret ∷ Secret
  , setCookies ∷ Map
      Name
      SetValue
  }

cookieStore ∷ Secret → HTTPure.Headers → CookieStore
cookieStore secret headers = CookieStore
  { clientCookies: defer \_ → clientCookies headers
  , secret
  , setCookies: mempty
  }

toSetCookieHeaders ∷ CookieStore → Array (Tuple String String)
toSetCookieHeaders (CookieStore { setCookies }) =
  setCookies # Map.toUnfoldable >>> map \(Tuple name { attributes, value }) →
      setCookieHeader name value attributes

lookup ∷ Name → CookieStore → Lazy (Maybe Value)
lookup name = map (map Array.NonEmpty.head) <<< lookup' name

lookup' ∷ Name → CookieStore → Lazy (Maybe Values)
lookup' name (CookieStore { clientCookies, secret, setCookies }) =
  defer \_ → signed >>= traverse unsign
  where
    unsign v = hush (Crypto.String.unsign secret v)
    signed = case name `Map.lookup` setCookies of
      Just { value, attributes } | attributes.expires /= Just epoch → Just (Array.NonEmpty.singleton value)
      Just _ → Nothing
      Nothing → name `Object.lookup` (Lazy.force clientCookies)

set ∷ Name → SetValue → CookieStore → Maybe CookieStore
set name { value, attributes } (CookieStore { clientCookies, secret, setCookies }) = ado
  value' ← hush (Crypto.String.sign secret value)
  in CookieStore
    { clientCookies
    , secret
    , setCookies: Map.insert name { attributes, value: value' } setCookies
    }

