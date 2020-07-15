module WebRow.Session.SessionStore.InMemory where

import Prelude

import Data.Lazy (Lazy)
import Data.Lazy (defer, force) as Lazy
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Ref (Ref)
import WebRow.KeyValueStore (Key)
import WebRow.KeyValueStore.InMemory (forRef) as KeyValueStore.InMemory
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (forKey, new) as SessionStore

new ∷ ∀ session. Ref (Map String session) → session → Maybe Key → Effect (SessionStore Effect session)
new ref defaultSession =
  let
    kv = KeyValueStore.InMemory.forRef ref
  in case _ of
    Just key → pure $ SessionStore.forKey defaultSession key kv
    Nothing → SessionStore.new defaultSession kv

-- | We can't be sure if Effect thunk is not run sequentially.
-- | Because it seems that this is the case.
lazy ∷ ∀ session. Ref (Map String session) → session → Lazy (Maybe Key) → Lazy (Effect (SessionStore Effect session))
lazy ref defaultSession mk = Lazy.defer \_ → do
  let
    kv = KeyValueStore.InMemory.forRef ref
  case Lazy.force mk of
    Just key → pure $ SessionStore.forKey defaultSession key kv
    Nothing → SessionStore.new defaultSession kv
