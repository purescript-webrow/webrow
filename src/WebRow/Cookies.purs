module WebRow.Cookies where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, filter, (:))
import Data.Array as Array
import Data.Either (Either, hush)
import Data.JSDate (JSDate, toUTCString)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NonEmpty
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Global.Unsafe (unsafeDecodeURIComponent, unsafeEncodeURIComponent)
import HTTPure as HTTPure
import Run (AFF, Run)
import Run.Reader (READER)
import WebRow.Crypto (Secret, sign, unsign)
import WebRow.Utils.Data.JSDate (epoch)

setCookie
  ∷ ∀ eff ctx
  . { name ∷ Name, value ∷ Value, attributes ∷ Attributes }
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      HTTPure.Headers
setCookie { name, value, attributes } = do
  signed ← sign value
  let signedWithAttributes = setCookieHeaderValue name signed attributes
  pure $ HTTPure.header setCookieHeaderKey signedWithAttributes

deleteCookie ∷ Name → HTTPure.Headers
deleteCookie name = HTTPure.header setCookieHeaderKey cookie
  where
    cookie = setCookieHeaderValue name ""
      $ defaultAttributes { expires = Just epoch }

getCookie
  ∷ ∀ eff ctx
  . Name
  → HTTPure.Headers
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      (Array String)
getCookie name headers = do
  let
    cookieValues = HTTPure.lookup headers cookieHeaderKey
      >>= parseCookies >>> hush
      >>= Object.lookup name
  case cookieValues of
    Nothing → pure []
    Just c → do
      cookieValues' ← for c $ unsign >>> map hush
      pure $ Array.fromFoldable >>> Array.catMaybes $ cookieValues'

-- | Flatten response
getCookie'
  ∷ ∀ eff ctx
  . Name
  → HTTPure.Headers
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      (Maybe String)
getCookie' name headers = Array.head <$> getCookie name headers

type Name = String
type Value = String
type Values = NonEmpty Array Value

data SameSite = Strict | Lax

type Attributes =
  { comment ∷ Maybe String
  , domain ∷ Maybe String
  , expires ∷ Maybe JSDate
  , httpOnly ∷ Boolean
  , maxAge ∷ Maybe Int
  , path ∷ Maybe String
  , sameSite ∷ Maybe SameSite
  , secure ∷ Boolean
  }

defaultAttributes ∷ Attributes
defaultAttributes =
  { comment: Nothing
  , domain: Nothing
  , expires: Nothing
  , httpOnly: false
  , maxAge: Nothing
  , path: Nothing
  , sameSite: Nothing
  , secure: false
  }

cookieHeaderKey ∷ String
cookieHeaderKey = "cookie"

setCookieHeaderKey ∷ String
setCookieHeaderKey = "Set-Cookie"

setCookieHeaderValue ∷ Name → Value → Attributes → String
setCookieHeaderValue key value { comment, expires, path, maxAge, domain, secure, httpOnly, sameSite } =
  [ Just $ assign (unsafeEncodeURIComponent key) (unsafeEncodeURIComponent value)
  , (assign "Comment" <<< unsafeEncodeURIComponent) <$> comment
  , (assign "Expires" <<< toUTCString) <$> expires
  , (assign "Max-Age" <<< show) <$> maxAge
  , assign "Domain" <$> domain
  , assign "Path" <$> path
  , assign "SameSite" <<< sameSiteSer <$> sameSite
  , if secure then Just "Secure" else Nothing
  , if httpOnly then Just "HttpOnly" else Nothing
  ]
  # catMaybes
  # joinWith ";"
 where
  assign k v = k <> "=" <> v
  sameSiteSer ∷ SameSite → String
  sameSiteSer Strict = "Strict"
  sameSiteSer Lax = "Lax"

parseCookies ∷ String → Either String (Object Values)
parseCookies s =
  splitPairs s
  <#> map toCookieMap
  <#> Object.fromFoldableWith combineCookies

splitPairs ∷ String → Either String (Array (Tuple Name String))
splitPairs =
  split (Pattern ";")
  >>> map trim
  >>> filter ((/=) "")
  >>> map (split (Pattern "=") >>> toPair)
  >>> sequence

toPair ∷ Array String → Either String (Tuple Name String)
toPair kv = case kv of
  [key, value] →
    pure $ Tuple (unsafeDecodeURIComponent key) (unsafeDecodeURIComponent value)
  parts →
    throwError ("Invalid cookie-pair: " <> joinWith " " parts)

toCookieMap ∷ Tuple Name String → Tuple Name Values
toCookieMap (Tuple name value) = Tuple name (NonEmpty value [])

combineCookies ∷ ∀ a. NonEmpty Array a → NonEmpty Array a → NonEmpty Array a
combineCookies xs ys =
  NonEmpty.head xs :| NonEmpty.head ys : NonEmpty.tail xs <> NonEmpty.tail ys
