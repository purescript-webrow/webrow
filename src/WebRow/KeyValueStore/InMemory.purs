module WebRow.KeyValueStore.InMemory where

import Prelude

import Data.Map (delete, insert, lookup) as Map
import Data.UUID (genUUID)
import Effect.Ref (modify, new, read) as Ref
import Run (Run)
import Run (liftEffect) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.KeyValueStore.Types (KeyValueRun)

type InMemory a = KeyValueRun (EffRow + ()) a

inMemoryStore ∷ ∀ a eff. Run (EffRow + eff) (InMemory a)
inMemoryStore = do
  ref ← Run.liftEffect $ Ref.new mempty
  let
    create = Run.liftEffect $ show <$> genUUID
    delete key = Run.liftEffect $ void $ Ref.modify (Map.delete key) ref
    get key = Run.liftEffect $ Ref.read ref >>= (Map.lookup key >>> pure)
    set key a = Run.liftEffect $ void $ Ref.modify (Map.insert key a) ref
  pure { create, delete, get, set }


-- type RedisStore a = DataStore Aff a
-- 
-- type RedisStoreConfig =
--   { connection ∷ Redis.Connection
--   , namespace ∷ String
--   , expiration ∷ Maybe Int
--   }
-- 
-- -- | XXX:
-- -- | * Replace ReadForeign / WriteForeign with EncodeJson / DecodeJson
-- redisStore
--   ∷ ∀ a
--   . ReadForeign a
--   ⇒ WriteForeign a
--   ⇒ RedisStoreConfig
--   → RedisStore a
-- redisStore config = { create, delete, get, set }
--   where
--   toKey k = toUTF8 $ config.namespace <> "." <> k
--   toValue a = toUTF8 <<< writeJSON $ a
-- 
--   create = show <$> liftEffect genUUID
--   delete key = del' config.connection [toKey key]
--   get key = do
--     v ← Redis.get config.connection (toKey key)
--     pure $ v >>= (fromUTF8 >>> readJSON >>> hush)
--   set key a =
--     Redis.set
--       config.connection
--       (toKey key)
--       (toValue a)
--       (Redis.EX <$> config.expiration)
--       Nothing
