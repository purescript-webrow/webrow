module WebRow.Applets.Registration.Render.Dummy where

-- import Run (AFF, Run(..))
-- import WebRow.Logging.Effect (LOGGER)
-- import WebRow.Applets.Registration (Response(..)) as Registration
-- 
-- render
--   ∷ ∀ eff
--   . Registration.Response
--   → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) String
-- render = case _ of
--   InvalidEmailSignature → "InvalidEmailSignature"
--   RenderPasswordForm → "Password From (please provide a password)"
--   CreateAccount email password → "Create account: " <> show email <> "and redirect"
--   EmailSent email → "Email sent to " <> show email
