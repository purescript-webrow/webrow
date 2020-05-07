module WebRow.DataStore where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run

data DataStoreF key val a
  = CreateKey (key → a)
  | Delete key a
  | Get key (Maybe val → a)
  | Set key val (val → a)
derive instance functorDataStoreF ∷ Functor (DataStoreF key val)

_store = SProxy ∷ SProxy "store"

type STORE key val = FProxy (DataStoreF key val)

createKey
  ∷ ∀ key val eff
  . Run ( store ∷ STORE key val | eff ) key
createKey = Run.lift _store (CreateKey identity)

delete
  ∷ ∀ key val eff
  . key
  → Run ( store ∷ STORE key val | eff ) Unit
delete key = Run.lift _store (Delete key unit)

get
  ∷ ∀ key val eff
  . key
  → Run ( store ∷ STORE key val | eff ) (Maybe val)
get key = Run.lift _store (Get key identity)

set
  ∷ ∀ key val eff
  . key
  → val
  → Run ( store ∷ STORE key val | eff ) val
set key val = Run.lift _store (Set key val identity)
