module WebRow.Session.SessionStore.InMemory where

import Prelude

import Data.Map (Map)
import Effect (Effect)
import Effect.Ref (Ref)
import WebRow.KeyValueStore.InMemory (forRef, new) as KeyValueStore.InMemory
import WebRow.Session.SessionStore (SessionStore, hoist)
import WebRow.Session.SessionStore (new) as SessionStore

new ∷ ∀ session. session → Effect (SessionStore Effect session)
new defaultSession =
  KeyValueStore.InMemory.new >>= SessionStore.new defaultSession

forRef ∷ ∀ session. Ref (Map String session) → session → Effect (SessionStore Effect session)
forRef ref defaultSession =
  KeyValueStore.InMemory.forRef ref # SessionStore.new defaultSession

lifted ∷ ∀ m session. Monad m ⇒ session → (Effect ~> m) → Effect (SessionStore m session)
lifted defaultSession liftInnerEffect = do
  ss ← new defaultSession
  pure $ hoist liftInnerEffect ss
