module WebRow.HTTP.Cookies
  ( Cookies
  , module Exports
  , _cookies
  , cookies
  , delete
  , lookup
  , lookup'
  , run
  , set
  )
  where

import Prelude

import Data.Foldable (for_)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Run (Run)
import Run.State (STATE, getAt, putAt, runStateAt)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Crypto (Crypto, secret)
import WebRow.HTTP.Cookies.CookieStore (CookieStore) as Exports
import WebRow.HTTP.Cookies.CookieStore (CookieStore, cookieStore, toSetCookieHeaders)
import WebRow.HTTP.Cookies.CookieStore (lookup, lookup', set) as CookieStore
import WebRow.HTTP.Cookies.Types (Attributes(..), Name, Value, Values, RequestCookies, ResponseCookies, SetValue, defaultAttributes) as Exports
import WebRow.HTTP.Cookies.Types (Name, SetValue, Value, Values, attributes)
import WebRow.HTTP.Request (Request)
import WebRow.HTTP.Request (headers) as Request
import WebRow.HTTP.Response (setHeader, SetHeader) as Response

type Cookies r = (cookies ∷ STATE CookieStore | r)

_cookies = SProxy ∷ SProxy "cookies"

cookies ∷ ∀ eff. Run (Cookies + eff) CookieStore
cookies = getAt _cookies

lookup' ∷ ∀ eff. Name → Run (Cookies + eff) (Lazy (Maybe Values))
lookup' name =
  CookieStore.lookup' name <$> cookies

lookup ∷ ∀ eff. Name → Run (Cookies + eff) (Lazy (Maybe Value))
lookup name =
  CookieStore.lookup name <$> cookies

-- | TODO: We should handle here cookie errors like "to large cookies" etc.
set ∷ ∀ eff. Name → SetValue → Run (Cookies + eff) Boolean
set name v = do
  cookies' ← CookieStore.set name v <$> cookies
  case cookies' of
    Just c → do
      putAt _cookies c
      pure true
    Nothing → pure false

delete ∷ ∀ eff. Name → Run (Cookies + eff) Boolean
delete name = set name { value: "", attributes: attributes _{ expires = Just epoch }}

-- | Useful for testing when we want
-- | to provide store directly and not
-- | print and parse headers.
runOnStore
  ∷ ∀ a eff
  . CookieStore
  → Run (Cookies + Response.SetHeader + eff) a
  → Run (Response.SetHeader + eff) a
runOnStore c action = do
  (Tuple cs a) ← runStateAt _cookies c action
  let
    headers = toSetCookieHeaders cs
  for_ headers \(Tuple k v) → do
    Response.setHeader k v
  pure a

run
  ∷ ∀ a eff
  . Run (Cookies + Crypto + Request + Response.SetHeader + eff) a
  → Run (Crypto + Request + Response.SetHeader + eff) a
run action = do
  s ← secret
  hs ← Request.headers
  runOnStore (cookieStore s hs) action
