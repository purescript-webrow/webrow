module WebRow.UUID where

import Prelude

import Data.UUID (UUID, genUUID)
import Run (FProxy, Run, on)
import Run (interpret, liftEffect, send) as Run
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)

newtype UuidF a = UuidF (UUID → a)

derive instance functorUUIDF ∷ Functor UuidF

type UUIDF
  = FProxy UuidF

type Uuid eff = (uuid ∷ UUIDF | eff)

_uuid = SProxy ∷ SProxy "uuid"

run ∷ ∀ eff. Run (EffRow + Uuid + eff) ~> Run (EffRow + eff)
run = Run.interpret (on _uuid handleUuid Run.send)
  where
    handleUuid ∷ ∀ b. UuidF b → Run (EffRow + eff) b
    handleUuid (UuidF next) = next <$> Run.liftEffect genUUID

