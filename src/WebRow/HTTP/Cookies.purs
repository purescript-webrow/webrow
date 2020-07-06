module WebRow.HTTP.Cookies
  ( Cookies
  , module Exports
  , cookies
  , delete
  , lookup
  , lookup'
  , set
  )
  where

import Prelude

import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Run (Run)
import Run.State (STATE, getAt, putAt)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Data.JSDate (epoch)
import WebRow.Cookies.CookieStore (CookieStore)
import WebRow.Cookies.CookieStore (CookieStore) as Exports
import WebRow.Cookies.CookieStore (lookup, lookup', set) as CookieStore
import WebRow.Crypto (Crypto)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP.Cookies.Types (Attributes, Name, Value, Values, SetValue, defaultAttributes) as Exports
import WebRow.HTTP.Cookies.Types (Name, Value, Values, SetValue, defaultAttributes)

type Cookies r = (cookies ∷ STATE CookieStore | r)

_cookies = SProxy ∷ SProxy "cookies"

cookies ∷ ∀ eff. Run (Cookies + eff) CookieStore
cookies = getAt _cookies

lookup' ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) (Lazy (Maybe Values))
lookup' name =
  CookieStore.lookup' <$> Crypto.secret <@> name <*> cookies

lookup ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) (Lazy (Maybe Value))
lookup name =
  CookieStore.lookup <$> Crypto.secret <@> name <*> cookies

set ∷ ∀ eff. Name → SetValue → Run (Cookies + Crypto + eff) Boolean
set name v = do
  cookies' ← CookieStore.set <$> Crypto.secret <@> name <@> v <*> cookies
  case cookies' of
    Just c → do
      putAt _cookies c
      pure true
    Nothing → pure false

delete ∷ ∀ eff. Name → Run (Cookies + Crypto + eff) Boolean
delete name = set name { value: "", attributes: defaultAttributes { expires = Just epoch }}

