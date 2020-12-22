module WebRow.KeyValueStore
  ( module Interpret
  , module Effect
  ) where

import WebRow.KeyValueStore.Effect (deleteAt, delete, getAt, get, putAt, put, newAt, new, _keyValueStore, Key, KEYVALUESTORE, KeyValueStore) as Effect
import WebRow.KeyValueStore.Interpret (runOnInterfaceAt, runOnInterface, Interface) as Interpret

