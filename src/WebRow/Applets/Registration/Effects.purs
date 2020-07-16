module WebRow.Applets.Registration.Effects where

import Prelude

import Run (FProxy, Run)
import Run (lift) as Run
import Type.Prelude (SProxy(..))
import WebRow.Applets.Auth.Types (Password)
import WebRow.Mailer (Email)

data RegistrationF a
  = EmailTakenF Email (Boolean → a)
  | RegisterF Email Password a

derive instance functorRegistrationF ∷ Functor RegistrationF

type REGISTER = FProxy RegistrationF

_registration = SProxy ∷ SProxy "registration"

type Registration r = (registration ∷ REGISTER | r)

emailTaken ∷ ∀ eff. Email → Run ( registration ∷ REGISTER | eff ) Boolean
emailTaken email = do
  Run.lift _registration (EmailTakenF email identity)

register ∷ ∀ eff. Email → Password → Run ( registration ∷ REGISTER | eff ) Unit
register email password = do
  Run.lift _registration (RegisterF email password unit)

