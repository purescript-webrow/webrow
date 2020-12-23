module WebRow.Session.SessionStore.InMemory where

import Prelude

import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.UUID (genUUID)
import Data.UUID (toString) as UUID
import Effect (Effect)
import Effect.Ref (Ref)
import WebRow.Cache (Key)
import WebRow.Cache.Interpret.InMemory (forRef) as Cache.InMemory
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (forKey) as SessionStore

new ∷ ∀ session. Ref (Map String session) → session → Lazy (Maybe Key) → Effect (SessionStore Effect session)
new ref defaultSession maybeKey = do
  let
    kv = Cache.InMemory.forRef ref
  newKey ← UUID.toString <$> genUUID
  let
    key = fromMaybe newKey <$> maybeKey
  pure $ SessionStore.forKey defaultSession key kv


