module WebRow.Registration.Interpret.Dummy where

-- interpret
--   ∷ ∀ eff a
--   . Run ( aff ∷ AFF, logger ∷ LOGGER, register ∷ REGISTER | eff ) a
--   → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) a
-- interpret = Run.interpret (Run.on _register handleRegister Run.send)
-- 
-- handleRegister
--   ∷ ∀ eff
--   . RegisterF ~> Run ( aff ∷ AFF, logger ∷ LOGGER | eff )
-- handleRegister = case _ of
--   AssertEmailNotTaken email next → do
--     pure next
-- 
-- handleRegisterResponse
--   ∷ ∀ eff
--   . RegisterResponse
--   → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) HTTPure.Response
-- handleRegisterResponse = case _ of
--   InvalidEmailSignature →
--     Run.liftAff $ HTTPure.badRequest "InvalidEmailSignature"
--   RenderPasswordForm →
--     ok "Password From (please provide a password)"
--   CreateAccount email password → do
--     -- TODO: register in the db and handle the result further
--     LogEff.err $ "Effect (CreateAccount " <> show email <> " " <> show password <> ") not handled"
--     Run.liftAff $ HTTPure.internalServerError "Registration failed"
--   EmailSent email →
--     ok $ "Email sent to " <> show email
--   where ok = Run.liftAff <<< HTTPure.ok
-- 
