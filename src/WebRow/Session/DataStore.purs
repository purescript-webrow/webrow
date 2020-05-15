module WebRow.Session.DataStore where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Run (AFF, Run)
import Run as Run
import WebRow.DataStore (DataStoreF(..), _store)
import WebRow.Logging.Effect (LOGGER)
import WebRow.Reader as WebRow
import WebRow.Response.Modify (MODIFY)
import WebRow.Session (SESSIONSTORE)

type Effects ctx eff =
  ( aff ∷ AFF
  , logger ∷ LOGGER
  , reader ∷ WebRow.READER ctx
  , modifyResponse ∷ MODIFY
  | eff
  )

runSessionStoreInMemory
  ∷ ∀ ctx a session eff
  . Store Effect UUID session
  → Run ( store ∷ SESSIONSTORE session | Effects ctx eff ) a
  → Run (                              | Effects ctx eff ) a
runSessionStoreInMemory store m =
  Run.interpret (Run.on _store (handler store lift) Run.send) m

lift ∷ forall eff a. Effect a → Run ( aff ∷ AFF | eff ) a
lift = Run.liftAff <<< liftEffect

handler
  ∷ ∀ m key val eff
  . Store m key val
  → (m ~> Run eff)
  → DataStoreF key val ~> Run eff
handler store liftM = case _ of
  Create onKey →
    liftM store.create <#> onKey
  Delete key next → do
    liftM (store.delete key)
    pure next
  Get key onMaybeVal → do
    liftM (store.get key) <#> onMaybeVal
  Set key val onVal → do
    liftM (store.set key val)
    pure $ onVal val

type Store m key val =
  { create ∷ m key
  , delete ∷ key → m Unit
  , get ∷ key → m (Maybe val)
  , set ∷ key → val → m Unit
  }

memoryStore ∷ ∀ val. Ref (Map.Map UUID val) → Store Effect UUID val
memoryStore ref =
  { create: genUUID
  , delete: \key → void $ Ref.modify (Map.delete key) ref
  , get: \key → Ref.read ref >>= (Map.lookup key >>> pure)
  , set: \key val → void $ Ref.modify (Map.insert key val) ref
  }
