module WebRow.KeyValueStore.Interpret.InCookies where

import Prelude
import Data.Lazy (force) as Lazy
import Run (Run, liftEffect)
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (Crypto)
import WebRow.HTTP.Cookies (Attributes, delete, lookup, set) as Cookies
import WebRow.HTTP.Cookies (Cookies)
import WebRow.KeyValueStore (Interface) as KeyValueStore
import WebRow.KeyValueStore.Interpret (Namespace, newKey)

type InCookies a
  = KeyValueStore.Interface (Run (Cookies + Crypto + EffRow + ())) a

inCookies ∷ Namespace → Cookies.Attributes → InCookies String
inCookies namespace attributes =
  let
    -- | TODO: Check if "value+key" < 4000 bytes
    put k value = Cookies.set k { value, attributes }

    new = liftEffect $ newKey namespace

    delete key = Cookies.delete key

    get key = Lazy.force <$> (Cookies.lookup key)
  in
    { delete, get, new, put }
