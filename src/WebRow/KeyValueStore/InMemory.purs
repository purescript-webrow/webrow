module WebRow.KeyValueStore.InMemory where

import Prelude

import Data.Map (delete, insert, lookup) as Map
import Effect (Effect)
import Effect.Ref (modify, new, read) as Ref
import WebRow.KeyValueStore.Types (KeyValueStore, hoist, newKey)

type InMemory a = KeyValueStore Effect a

new ∷ ∀ a. Effect (InMemory a)
new = do
  ref ← Ref.new mempty
  let
    key = newKey ""
    delete k = (void $ Ref.modify (Map.delete k) ref) *> pure true
    get k = Ref.read ref >>= (Map.lookup k >>> pure)
    put k v = do
      void $ Ref.modify (Map.insert k v) ref
      pure true
  pure { delete, get, new: key, put }


lifted ∷ ∀ a m. Monad m ⇒ (Effect ~> m) → m (KeyValueStore m a)
lifted liftEffect = do
  kv ← liftEffect new
  pure $ hoist liftEffect kv
