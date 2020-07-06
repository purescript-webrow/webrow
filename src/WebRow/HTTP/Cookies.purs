module WebRow.HTTP.Cookies
  ( Cookies
  , module Exports
  , _cookies
  , cookies
  , delete
  , lookup
  , lookup'
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
import WebRow.HTTP.Cookies.Types (Attributes, Name, Value, Values, SetValue, defaultAttributes) as Exports
import WebRow.HTTP.Cookies.Types (Name, Value, Values, SetValue, defaultAttributes)
import WebRow.HTTP.Request (Request, headers)
import WebRow.HTTP.Response.SetHeader (setHeader, SetHeader)

type Cookies r = (cookies ∷ STATE CookieStore | r)

_cookies = SProxy ∷ SProxy "cookies"

cookies ∷ ∀ eff. Run (Cookies + eff) CookieStore
cookies = getAt _cookies

lookup' ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) (Lazy (Maybe Values))
lookup' name =
  CookieStore.lookup' name <$> cookies

lookup ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) (Lazy (Maybe Value))
lookup name =
  CookieStore.lookup name <$> cookies

set ∷ ∀ eff. Name → SetValue → Run (Cookies + Crypto + eff) Boolean
set name v = do
  cookies' ← CookieStore.set name v <$> cookies
  case cookies' of
    Just c → do
      putAt _cookies c
      pure true
    Nothing → pure false

delete ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) Boolean
delete name = set name { value: "", attributes: defaultAttributes { expires = Just epoch }}

-- | Useful for testing when we want
-- | to provide store directly and not
-- | print and parse headers.
runOnStore ∷ ∀ a eff. CookieStore → Run (Cookies + SetHeader + eff) a → Run (SetHeader + eff) a
runOnStore c action = do
  (Tuple cs a) ← runStateAt _cookies c action
  let
    headers = toSetCookieHeaders cs
  for_ headers \(Tuple k v) → do
    setHeader k v
  pure a

run
  ∷ ∀ a eff
  . Run (Cookies + Crypto + Request + SetHeader + eff) a
  → Run (Crypto + Request + SetHeader + eff) a
run action = do
  s ← secret
  hs ← headers
  runOnStore (cookieStore s hs) action
