module WebRow.Applets.Registration.Effects where

import Prelude

import Run (Run)
import Run (lift) as Run
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Types (Password)
import WebRow.Mailer (Email)

data Registration a
  = EmailTakenF Email (Boolean → a)
  | RegisterF Email Password a

derive instance functorRegistration ∷ Functor Registration

_registration = SProxy ∷ SProxy "registration"

type REGISTRATION r
  = ( registration ∷ Registration | r )

emailTaken ∷ ∀ eff. Email → Run ( REGISTRATION + eff ) Boolean
emailTaken email = do
  Run.lift _registration (EmailTakenF email identity)

register ∷ ∀ eff. Email → Password → Run ( REGISTRATION + eff ) Unit
register email password = do
  Run.lift _registration (RegisterF email password unit)
