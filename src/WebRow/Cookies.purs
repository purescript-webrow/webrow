module WebRow.Cookies where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, filter, (:))
import Data.Either (Either)
import Data.JSDate (JSDate, toUTCString)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NonEmpty
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Global.Unsafe (unsafeDecodeURIComponent, unsafeEncodeURIComponent)

type Name = String
type Value = String
type Values = NonEmpty Array Value

data SameSite = Strict | Lax

type CookieAttributes =
  { comment ∷ Maybe String
  , domain ∷ Maybe String
  , expires ∷ Maybe JSDate
  , httpOnly ∷ Boolean
  , maxAge ∷ Maybe Int
  , path ∷ Maybe String
  , sameSite ∷ Maybe SameSite
  , secure ∷ Boolean
  }

defaultCookieAttributes ∷ CookieAttributes
defaultCookieAttributes =
  { comment: Nothing
  , domain: Nothing
  , expires: Nothing
  , httpOnly: false
  , maxAge: Nothing
  , path: Nothing
  , sameSite: Nothing
  , secure: false
  }

setCookieHeaderValue ∷ Name → Value → CookieAttributes → String
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
