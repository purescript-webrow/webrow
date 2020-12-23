module WebRow.Cache.Interpret.InCookies where

import Prelude

import Data.Lazy (force) as Lazy
import Run (Run)
import Type.Row (type (+))
import WebRow.Cache.Interpret (Interface)
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (Crypto)
import WebRow.HTTP.Cookies (Attributes, delete, lookup, set) as Cookies
import WebRow.HTTP.Cookies (Cookies)

type InCookies a
  = Interface (Run (Cookies + Crypto + EffRow + ())) Cookies.Attributes a

inCookies ∷ InCookies String
inCookies =
  let
    -- | TODO: Check if "value+key" < 4000 bytes
    insert k attrs value = Cookies.set k { attributes: attrs, value }

    delete key = Cookies.delete key

    lookup key = Lazy.force <$> (Cookies.lookup key)
  in
    { delete, insert, lookup }
