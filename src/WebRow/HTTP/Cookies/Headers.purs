module WebRow.HTTP.Cookies.Headers where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, filter)
import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.Either (Either, hush)
import Data.JSDate (toUTCString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import HTTPure (Headers, lookup) as HTTPure
import Run (EFFECT, Run)
import Type.Row (type (+))
import WebRow.Contrib.JSURI (unsafeDecodeURIComponent, unsafeEncodeURIComponent)
import WebRow.Crypto (CRYPTO)
import WebRow.HTTP.Cookies.Types (Attributes(..), Name, RequestCookies, SameSite(..), SetValue, Value, Values)
import WebRow.HTTP.Response (SETHEADER, setHeader) as HTTP.Response

requestCookies ∷ HTTPure.Headers → RequestCookies
requestCookies hs =
  fromMaybe mempty
    $ hush
    <<< parseCookies
    =<< HTTPure.lookup hs cookieHeaderKey

setCookie ∷
  ∀ eff.
  Name →
  SetValue →
  Run (EFFECT + CRYPTO + HTTP.Response.SETHEADER + eff) Unit
setCookie name { value, attributes } = do
  let
    h = setCookieHeaderValue name value attributes
  HTTP.Response.setHeader setCookieHeaderKey h

cookieHeaderKey ∷ String
cookieHeaderKey = "cookie"

setCookieHeaderKey ∷ String
setCookieHeaderKey = "Set-Cookie"

setCookieHeader ∷ Name → Value → Attributes → Tuple String String
setCookieHeader n v attrs = Tuple setCookieHeaderKey (setCookieHeaderValue n v attrs)

-- | XXX: Add cookie size check here
setCookieHeaderValue ∷ Name → Value → Attributes → String
setCookieHeaderValue key value (Attributes { comment, expires, path, maxAge, domain, secure, httpOnly, sameSite }) =
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

parseCookies ∷ String → Either String RequestCookies
parseCookies s =
  splitPairs s
    <#> map toCookieMap
    <#> Object.fromFoldableWith append

splitPairs ∷ String → Either String (Array (Tuple Name String))
splitPairs =
  split (Pattern ";")
    >>> map trim
    >>> filter ((/=) "")
    >>> map (split (Pattern "=") >>> toPair)
    >>> sequence

toPair ∷ Array String → Either String (Tuple Name String)
toPair kv = case kv of
  [ key, value ] → pure $ Tuple (unsafeDecodeURIComponent key) (unsafeDecodeURIComponent value)
  parts → throwError ("Invalid cookie-pair: " <> joinWith " " parts)

toCookieMap ∷ Tuple Name String → Tuple Name Values
toCookieMap (Tuple name value) = Tuple name (Array.NonEmpty.singleton value)
