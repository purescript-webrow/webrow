module WebRow.Applets.Registration.Testing.Messages where

import Prelude

import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import WebRow.Applets.Registration.Forms (PasswordsInput)
import WebRow.Mailer (Email(..))

type Printers =
  ( emailTaken ∷ Email → String
  , passwordsMismatch ∷ PasswordsInput → String
  )

printers ∷ { | Printers }
printers =
  { emailTaken: \(Email email) →
      ( "Accout for a given email is already registered. "
      <> " Please use password reset option if you are not able to "
      <> " access your account."
      )
  , passwordsMismatch: \_ →
      "Passwords don't match. Please ensure that both provided password are equal."
  }

registration
  ∷ ∀ r r' r''
  . Row.Union r Printers r'
  ⇒ Row.Nub r' r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
registration = Record.Builder.merge printers
