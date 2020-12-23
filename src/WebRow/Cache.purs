module WebRow.Cache
  ( module Interpret
  , module Effect
  ) where

import WebRow.Cache.Effect (deleteAt, delete, lookupAt, lookup, insertAt, insert, _cache, Key, CACHE, Cache) as Effect
import WebRow.Cache.Interpret (runOnInterfaceAt, runOnInterface, Interface) as Interpret

