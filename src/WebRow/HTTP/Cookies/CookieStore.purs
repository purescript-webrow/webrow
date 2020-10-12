module WebRow.HTTP.Cookies.CookieStore where

import Prelude
import Data.Argonaut (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, singleton) as Array.NonEmpty
import Data.Either (hush)
import Data.Lazy (Lazy, defer)
import Data.Lazy (force) as Lazy
import Data.Map (insert, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import HTTPure as HTTPure
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Crypto (Secret)
import WebRow.Crypto.Jwt.Node (sign, unsign) as Crypto.Jwt.Node
import WebRow.Crypto.Jwt.Node.String (sign, unsign) as Crypto.String
import WebRow.HTTP.Cookies.Headers (requestCookies, setCookieHeader)
import WebRow.HTTP.Cookies.Types (Attributes) as Cookies
import WebRow.HTTP.Cookies.Types (Name, RequestCookies, SetValue, Value, Values, ResponseCookies)

newtype CookieStore
  = CookieStore
  { requestCookies ∷ Lazy RequestCookies
  , secret ∷ Secret
  , responseCookies ∷ ResponseCookies
  }

cookieStore ∷ Secret → HTTPure.Headers → CookieStore
cookieStore secret headers =
  CookieStore
    { requestCookies: defer \_ → requestCookies headers
    , secret
    , responseCookies: mempty
    }

toSetCookieHeaders ∷ CookieStore → Array (Tuple String String)
toSetCookieHeaders (CookieStore { responseCookies }) =
  responseCookies # Map.toUnfoldable
    >>> map \(Tuple name { attributes, value }) →
        setCookieHeader name value attributes

lookup ∷ Name → CookieStore → Lazy (Maybe Value)
lookup name = map (map Array.NonEmpty.head) <<< lookup' name

lookup' ∷ Name → CookieStore → Lazy (Maybe Values)
lookup' name (CookieStore { requestCookies, secret, responseCookies }) = defer f
  where
  f _ = signed >>= traverse unsign
    where
    unsign v = hush (Crypto.String.unsign secret v)

    signed = case name `Map.lookup` responseCookies of
      Just { value, attributes }
        | (unwrap attributes).expires /= Just epoch → Just (Array.NonEmpty.singleton value)
      Just _ → Nothing
      Nothing → name `Object.lookup` (Lazy.force requestCookies)

-- | Because we are using JWT for payload encoding in cookie store we
-- | are able to handle Json directly and we don't really have to
-- | reencode anything two times.
lookupJson ∷ Name → CookieStore → Lazy (Maybe Json)
lookupJson name = map (map Array.NonEmpty.head) <<< lookupJson' name

lookupJson' ∷ Name → CookieStore → Lazy (Maybe (NonEmptyArray Json))
lookupJson' name (CookieStore { requestCookies, secret, responseCookies }) = defer f
  where
  f _ = signed >>= traverse unsign
    where
    unsign v = hush (Crypto.Jwt.Node.unsign secret v)

    signed = case name `Map.lookup` responseCookies of
      Just { value, attributes }
        | (unwrap attributes).expires /= Just epoch → Just (Array.NonEmpty.singleton value)
      Just _ → Nothing
      Nothing → name `Object.lookup` (Lazy.force requestCookies)

set ∷ Name → SetValue → CookieStore → Maybe CookieStore
set name { value, attributes } (CookieStore { requestCookies, secret, responseCookies }) = ado
  value' ← hush (Crypto.String.sign secret value)
  in CookieStore
    { requestCookies
    , secret
    , responseCookies: Map.insert name { attributes, value: value' } responseCookies
    }

-- | Because we are using jwt for payload encoding
-- | we can handle json directly.
setJson ∷ Name → { json ∷ Json, attributes ∷ Cookies.Attributes } → CookieStore → Maybe CookieStore
setJson name { json, attributes } (CookieStore { requestCookies, secret, responseCookies }) = ado
  value' ← hush (Crypto.Jwt.Node.sign secret json)
  in CookieStore
    { requestCookies
    , secret
    , responseCookies: Map.insert name { attributes, value: value' } responseCookies
    }
