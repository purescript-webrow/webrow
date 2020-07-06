module WebRow.KeyValueStore.InMemory where

import Prelude

import Data.Map (delete, insert, lookup) as Map
import Effect (Effect)
import Effect.Ref (modify, new, read) as Ref
import WebRow.KeyValueStore.Types (KeyValueStore, newKey)

type InMemory a = KeyValueStore Effect a

inMemory ∷ ∀ a. Effect (InMemory a)
inMemory = do
  ref ← Ref.new mempty
  let
    new = newKey ""
    delete key = (void $ Ref.modify (Map.delete key) ref) *> pure true
    get key = Ref.read ref >>= (Map.lookup key >>> pure)
    put k v = do
      void $ Ref.modify (Map.insert k v) ref
      pure true
  pure { delete, get, new, put }

