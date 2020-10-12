module WebRow.Applets.Auth.Testing.Messages where

import Prelude
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import WebRow.Applets.Auth.Forms (AuthPayload)

type Printers
  = ( authFailed ∷ AuthPayload → String )

printers ∷ { | Printers }
printers =
  { authFailed:
      \_ →
        ( "Email or password is incorrect. Please try again or"
            <> " use password reset option."
        )
  }

auth ∷
  ∀ r r' r''.
  Row.Union r Printers r' ⇒
  Row.Nub r' r'' ⇒
  Record.Builder.Builder { | r } { | r'' }
auth = Record.Builder.merge printers
