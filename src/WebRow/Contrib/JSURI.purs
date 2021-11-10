module WebRow.Contrib.JSURI where

import Prelude

import Data.Maybe (fromJust)
import JSURI (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)

unsafeDecodeURIComponent :: String -> String
unsafeDecodeURIComponent s = unsafePartial $ fromJust $ decodeURIComponent s

unsafeEncodeURIComponent :: String -> String
unsafeEncodeURIComponent s = unsafePartial $ fromJust $ encodeURIComponent s
