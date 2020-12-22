module WebRow.KeyValueStore.Interpret.InMemory where

import Prelude
import Data.Map (Map)
import Data.Map (delete, insert, lookup) as Map
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (modify, new, read) as Ref
import WebRow.KeyValueStore.Interpret (Interface, newKey)

type InMemory a
  = Interface Effect a

-- | TODO: Provide also efficient JS Map reference
-- | based implementation done through mutable
-- | reference.
new ∷ ∀ a. Effect (InMemory a)
new = forRef <$> Ref.new mempty

forRef ∷ ∀ a. Ref (Map String a) → InMemory a
forRef ref =
  let
    key = newKey ""

    delete k = (void $ Ref.modify (Map.delete k) ref) *> pure true

    get k = do
      m ← Ref.read ref
      pure $ Map.lookup k m

    put k v = do
      void $ Ref.modify (Map.insert k v) ref
      pure true
  in
    { delete, get, new: key, put }
