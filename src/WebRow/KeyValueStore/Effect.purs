module WebRow.DataStore.Effect where

import Prelude
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run
import WebRow.KeyValueStore.Types (Key)

data KeyValueStoreF val a
  = DeleteF Key a
  | GetF Key (Maybe val → a)
  | ModifyF Key (val → Maybe val) Boolean
  | NewF val (Maybe Key → a)

derive instance functorDataStoreF ∷ Functor (KeyValueStoreF val)

-- type KeyValueStore =
--   { delete: \k → Run.lift <<< DeleteF k
--   , get: \k → 
-- _store = SProxy ∷ SProxy "store"
-- 
-- type STORE key val = FProxy (DataStoreF key val)
-- create
--   ∷ ∀ key val eff
--   . Run ( store ∷ STORE key val | eff ) key
-- create = Run.lift _store (Create identity)
-- 
-- delete
--   ∷ ∀ key val eff
--   . key
--   → Run ( store ∷ STORE key val | eff ) Unit
-- delete key = Run.lift _store (Delete key unit)
-- 
-- get
--   ∷ ∀ key val eff
--   . key
--   → Run ( store ∷ STORE key val | eff ) (Maybe val)
-- get key = Run.lift _store (Get key identity)
-- 
-- set
--   ∷ ∀ key val eff
--   . key
--   → val
--   → Run ( store ∷ STORE key val | eff ) val
-- set key val = Run.lift _store (Set key val identity)
