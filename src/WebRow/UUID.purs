module WebRow.UUID where

import Prelude

import Data.UUID (UUID)
import Data.UUID (genUUID) as UUID
import Run (Run, on)
import Run (interpret, liftEffect, send) as Run
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)

newtype GenUUID a = GenUUID (UUID → a)

derive instance functorUUIDF ∷ Functor GenUUID

type GENUUID eff = (genUUID ∷ GenUUID | eff)

_genUUID = Proxy ∷ Proxy "genUUID"

run ∷ ∀ eff. Run (EffRow + GENUUID + eff) ~> Run (EffRow + eff)
run = Run.interpret (on _genUUID handleUuid Run.send)
  where
    handleUuid ∷ ∀ b. GenUUID b → Run (EffRow + eff) b
    handleUuid (GenUUID next) = next <$> Run.liftEffect UUID.genUUID

