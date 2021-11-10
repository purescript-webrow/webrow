module WebRow.Cache.Interpret.InMemory where

import Prelude
import Data.Map (Map)
import Data.Map (delete, empty, insert, lookup) as Map
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (modify, new, read) as Ref
import WebRow.Cache.Interpret (Interface)

type InMemory attrs a
  = Interface Effect attrs a

-- | TODO: Provide also efficient JS Map reference
-- | based implementation done through mutable
-- | reference.
new ∷ ∀ a attrs. Effect (InMemory attrs a)
new = forRef <$> Ref.new Map.empty

forRef ∷ ∀ a attrs. Ref (Map String a) → InMemory attrs a
forRef ref =
  let
    delete k = (void $ Ref.modify (Map.delete k) ref) *> pure true

    lookup k = do
      m ← Ref.read ref
      pure $ Map.lookup k m

    insert k attrs v = do
      void $ Ref.modify (Map.insert k v) ref
      pure true
  in
    { delete, insert, lookup }
