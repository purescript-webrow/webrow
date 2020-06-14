module WebRow.Applets.Registration.Effects where

import Prelude

import Run (FProxy, Run)
import Run (lift) as Run
import Type.Prelude (SProxy(..))
import WebRow.Mailer (Email)

data RegistrationF a = EmailTaken Email (Boolean → a)

derive instance functorRegistrationF ∷ Functor RegistrationF

type REGISTER = FProxy RegistrationF

_registration = SProxy ∷ SProxy "registration"

type Registration r = (registration ∷ REGISTER | r)

emailTaken ∷ ∀ eff. Email → Run ( registration ∷ REGISTER | eff ) Boolean
emailTaken email = do
  Run.lift _registration (EmailTaken email identity)

-- type Effects ctx res routes user eff widgets =
--   ( aff ∷ AFF
--   , auth ∷ AUTH user
--   , logger ∷ LOGGER
--   , mailer ∷ MAILER
--   , reader ∷ WebRow.Reader.READER ctx
--   , registration ∷ REGISTER
--   , response ∷ RESPONSE (ResponseRow widgets res)
--   , route ∷ ROUTE (Routes.RouteRow + Auth.Routes.RouteRow + routes)
--   | eff
--   )
-- 
