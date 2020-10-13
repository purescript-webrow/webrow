module WebRow.Session.SessionStore.InMemory where

import Prelude

import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Ref (Ref)
import WebRow.KeyValueStore (Key)
import WebRow.KeyValueStore.InMemory (forRef) as KeyValueStore.InMemory
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (forKey) as SessionStore

new ∷ ∀ session. Ref (Map String session) → session → Lazy (Maybe Key) → Effect (SessionStore Effect session)
new ref defaultSession maybeKey = do
  let
    kv = KeyValueStore.InMemory.forRef ref
  newKey ← kv.new
  let
    key = fromMaybe newKey <$> maybeKey
  pure $ SessionStore.forKey defaultSession key kv


