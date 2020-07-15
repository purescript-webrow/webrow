module WebRow.Session.SessionStore.InMemory where

import Prelude

import Data.Lazy (Lazy)
import Data.Lazy (defer, force) as Lazy
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
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

lazy ∷ ∀ session. Ref (Map String session) → session → Lazy (Maybe Key) → Effect (Lazy (SessionStore Effect session))
lazy ref defaultSession mk = sequence $ Lazy.defer \_ →
  let
    kv = KeyValueStore.InMemory.forRef ref
  in case Lazy.force mk of
    Just key → pure $ SessionStore.forKey defaultSession key kv
    Nothing → SessionStore.new defaultSession kv
