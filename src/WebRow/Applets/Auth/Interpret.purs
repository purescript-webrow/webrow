module WebRow.Auth.Interpret where

import Prelude

import Effect.Random (random)
import Run (EFFECT, Run, liftEffect)
import Run (interpret, on, send) as Run
import Selda (selectFrom)
import Selda as Selda
import WebRow.Applets.Auth.Effects (AuthF(..))

-- interpret
--   ∷ ∀ eff
--   . Run
--     ( register ∷ REGISTER
--     | eff
--     )
--    ~> Run (effect ∷ EFFECT | eff)
-- interpret = Run.interpret (Run.on _register handler Run.send)


-- handler
  -- ∷ ∀ eff
  -- . RegisterF ~> Run (effect ∷ EFFECT | eff)
-- handler (Authenticate email password k) = do
--   selectFrom usersTable
--   v ← liftEffect random
--   pure (next (v > 0.5))

