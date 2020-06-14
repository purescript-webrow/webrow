module WebRow.Applets.Registration.Responses where

import WebRow.Applets.Auth.Types (Password)
import WebRow.Forms (Layout) as Forms
import WebRow.Mailer (Email)
import WebRow.Route (FullUrl)

data ConfirmationResponse (widgets ∷ # Type)
  = ConfirmationSucceeded Email Password
  | InvalidEmailSignature
  | InitialPasswordForm (Forms.Layout widgets)
  | EmailRegisteredInbetween Email
  | PasswordValidationFailed (Forms.Layout widgets)

data RegisterEmailResponse (widgets ∷ # Type)
  = EmailSent Email FullUrl
  | EmailValidationFailed (Forms.Layout widgets)
  | InitialEmailForm (Forms.Layout widgets)

data ChangeEmailResponse (widgets ∷ # Type)
  = ChangeEmailInitialForm (Forms.Layout widgets)

data Response (widgets ∷ # Type)
  = ConfirmationResponse (ConfirmationResponse widgets)
  | RegisterEmailResponse (RegisterEmailResponse widgets)
  | ChangeEmailResponse (ChangeEmailResponse widgets)

