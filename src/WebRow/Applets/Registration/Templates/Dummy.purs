module WebRow.Applets.Registration.Render.Dummy where

import Prelude

import Global.Unsafe (unsafeStringify)
import Run (AFF, Run(..))
import WebRow.Applets.Registration (ConfirmationResponse(..), RegisterEmailResponse(..))
import WebRow.Applets.Registration (Response(..))
import WebRow.Logging.Effect (LOGGER)

render
  ∷ ∀ eff
  . Response
  → String
render = case _ of
  RegisterEmailResponse r → case r of
    EmailAlreadyRegistered email → "Email already registered:" <> show email
    EmailSent email → "Email sent to " <> show email

  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
    EmailRegisteredInbetween _ -> "Email registered inbetween"
    InitialPasswordForm formLayout → "Please setup your password:" <> unsafeStringify formLayout
    InvalidEmailSignature → "InvalidEmailSignature"
    PasswordValidationFailed formLayout → "Password From: " <> unsafeStringify formLayout

