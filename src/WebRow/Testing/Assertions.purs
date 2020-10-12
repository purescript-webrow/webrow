module WebRow.Testing.Assertions where

import Prelude
import Run (Run, liftEffect)
import Test.Spec.Assertions (shouldEqual) as Assertions
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)

shouldEqual ∷ ∀ a eff. Show a ⇒ Eq a ⇒ a → a → Run (EffRow + eff) Unit
shouldEqual expected given = liftEffect $ Assertions.shouldEqual expected given
