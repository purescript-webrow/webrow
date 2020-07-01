module WebRow.HTTP.Cookies where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (fromString) as Argonaut
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
import Run (Run)
import Type.Row (type (+))
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (Crypto, signJson, unsign)
import WebRow.HTTP.Request (Request, headers)
import WebRow.HTTP.Response.SetHeader (SetHeader, setHeader) as HTTP


-- | TODO
-- | Currently we provide really simple but limited API
-- | for cookie handling. It is impossible to chain updates
-- | of a single cookie.
-- | We should think about better implementation.
-- | Something like "lazy store" + conditional write
-- | should work better because cookie management can be
-- | seen as a statefull chain of updates during
-- | request life cycle.
setCookie
  ∷ ∀ eff
  . { name ∷ String, value ∷ String, attributes ∷ Attributes }
  → Run (EffRow + Crypto + HTTP.SetHeader + eff) Unit
setCookie { name, value, attributes } = do
  signed ← signJson (Argonaut.fromString value)
  let signedWithAttributes = setCookieHeaderValue name signed attributes
  HTTP.setHeader setCookieHeaderKey signedWithAttributes

deleteCookie ∷ ∀ eff. Name → Run ( HTTP.SetHeader + eff) Unit
deleteCookie name = HTTP.setHeader setCookieHeaderKey cookie
  where
    cookie = setCookieHeaderValue name ""
      $ defaultAttributes { expires = Just epoch }

getCookie
  ∷ ∀ eff
  . Name
  → Run (EffRow + Crypto + Request + eff) (Array String)
getCookie name = do
  hs ← headers
  let
    cookieValues = HTTPure.lookup hs cookieHeaderKey
      >>= parseCookies >>> hush
      >>= Object.lookup name
  case cookieValues of
    Nothing → pure []
    Just c → do
      cookieValues' ← for c $ unsign >>> map hush
      pure $ Array.fromFoldable >>> Array.catMaybes $ cookieValues'

-- | Flatten response
getCookie'
  ∷ ∀ eff
  . Name
  → Run (EffRow + Crypto + Request + eff) (Maybe String)
getCookie' name = Array.head <$> getCookie name

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
