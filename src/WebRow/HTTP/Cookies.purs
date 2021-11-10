module WebRow.HTTP.Cookies
  ( Cookies
  , COOKIES
  , module Exports
  , _cookies
  , cookies
  , delete
  , lookup
  , lookup'
  , lookupJson
  , lookupJson'
  , run
  , runOnStore
  , set
  , setJson
  ) where

import Prelude
import Data.Argonaut (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (for_)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Run (Run)
import Run.State (State, getAt, putAt, runStateAt)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Crypto (CRYPTO, secret)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..)) as Exports
import WebRow.HTTP.Cookies.CookieStore (CookieStore, cookieStore, toSetCookieHeaders)
import WebRow.HTTP.Cookies.CookieStore (lookup, lookup', lookupJson, lookupJson', set, setJson) as CookieStore
import WebRow.HTTP.Cookies.Types (Attributes, Name, SetValue, Value, Values, attributes)
import WebRow.HTTP.Cookies.Types (attributes, Attributes(..), Name, Value, Values, RequestCookies, ResponseCookies, SetValue, defaultAttributes) as Exports
import WebRow.HTTP.Request (REQUEST)
import WebRow.HTTP.Request (headers) as Request
import WebRow.HTTP.Response (setHeader, SETHEADER) as Response

type Cookies = State CookieStore

type COOKIES r = ( cookies ∷ Cookies | r )

_cookies = SProxy ∷ SProxy "cookies"

cookies ∷ ∀ eff. Run (COOKIES + eff) CookieStore
cookies = getAt _cookies

lookup ∷ ∀ eff. Name → Run (COOKIES + eff) (Lazy (Maybe Value))
lookup name = CookieStore.lookup name <$> cookies

lookup' ∷ ∀ eff. Name → Run (COOKIES + eff) (Lazy (Maybe Values))
lookup' name = CookieStore.lookup' name <$> cookies

lookupJson ∷ ∀ eff. Name → Run (COOKIES + eff) (Lazy (Maybe Json))
lookupJson name = CookieStore.lookupJson name <$> cookies

lookupJson' ∷ ∀ eff. Name → Run (COOKIES + eff) (Lazy (Maybe (NonEmptyArray Json)))
lookupJson' name = CookieStore.lookupJson' name <$> cookies

-- | TODO: We should handle here cookie errors like "to large cookies" etc.
set ∷ ∀ eff. Name → SetValue → Run (COOKIES + eff) Boolean
set name v = do
  cookies' ← CookieStore.set name v <$> cookies
  case cookies' of
    Just c → do
      putAt _cookies c
      pure true
    Nothing → pure false

setJson ∷ ∀ eff. Name → { json ∷ Json, attributes ∷ Attributes } → Run (COOKIES + eff) Boolean
setJson name v = do
  cookies' ← CookieStore.setJson name v <$> cookies
  case cookies' of
    Just c → do
      putAt _cookies c
      pure true
    Nothing → pure false

delete ∷ ∀ eff. Name → Run (COOKIES + eff) Boolean
delete name = set name { value: "", attributes: attributes _ { expires = Just epoch } }

-- | Useful for testing when we want
-- | to provide store directly and not
-- | print and parse headers.
runOnStore ∷
  ∀ a eff.
  CookieStore →
  Run (COOKIES + Response.SETHEADER + eff) a →
  Run (Response.SETHEADER + eff) a
runOnStore c action = do
  (Tuple cs a) ← runStateAt _cookies c action
  let
    headers = toSetCookieHeaders cs
  for_ headers \(Tuple k v) → do
    Response.setHeader k v
  pure a

run ∷
  ∀ a eff.
  Run (COOKIES + CRYPTO + REQUEST + Response.SETHEADER + eff) a →
  Run (CRYPTO + REQUEST + Response.SETHEADER + eff) a
run action = do
  s ← secret
  hs ← Request.headers
  runOnStore (cookieStore s hs) action
