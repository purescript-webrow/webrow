module WebRow.Applets.Registration.Effects where

import Prelude

import Run (AFF, FProxy, Run)
import Run (lift) as Run
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (AUTH)
import WebRow.Applets.Auth.Routes (RouteRow) as Auth.Routes
import WebRow.Applets.Registration.Responses (ResponseRow)
import WebRow.Applets.Registration.Routes (RouteRow) as Routes
import WebRow.Applets.Registration.Types (_register)
import WebRow.Logging.Effect (LOGGER)
import WebRow.Mailer (Email, MAILER)
import WebRow.Reader (READER) as WebRow.Reader
import WebRow.Response (RESPONSE)
import WebRow.Route (ROUTE)

data RegisterF a = EmailTaken Email (Boolean → a)

derive instance functorRegisterF ∷ Functor RegisterF

type REGISTER = FProxy RegisterF

emailTaken ∷ ∀ eff. Email → Run ( aff ∷ AFF, register ∷ REGISTER | eff ) Boolean
emailTaken email = do
  Run.lift _register (EmailTaken email identity)

type Effects ctx res routes user eff widgets =
  ( aff ∷ AFF
  , auth ∷ AUTH user
  , logger ∷ LOGGER
  , mailer ∷ MAILER
  , reader ∷ WebRow.Reader.READER ctx
  , register ∷ REGISTER
  , response ∷ RESPONSE (ResponseRow widgets res)
  , route ∷ ROUTE (Routes.RouteRow + Auth.Routes.RouteRow + routes)
  | eff
  )

