module WebRow.Applets.Auth.Forms where

import Prelude

import Data.Either (note)
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (Msg, error) as Batteries
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Validator (liftFnMV) as Validator
import Run (Run)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (authenticate) as Effects
import WebRow.Applets.Auth.Effects (User, AUTH)
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Forms (Uni, Layout) as Forms
import WebRow.Forms.Uni (Builder, build, emailInputBuilder, passwordInputBuilder, sectionValidator) as Uni
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Widgets (TextInput)
import WebRow.Mailer (Email)

_authFailed = Proxy ∷ Proxy "authFailed"

type Msg = Batteries.Msg (AuthFailed + MissingValue + InvalidEmailFormat + ())

type Widgets
  = (TextInput () + ())

type LoginLayout
  = Forms.Layout Msg Widgets

type AuthPayload
  = { email ∷ Email, password ∷ Password }

type AuthFailed r
  = ( authFailed ∷ AuthPayload | r )

loginForm ::
  forall eff user.
  Forms.Uni
    (Run (AUTH user + eff))
    Msg
    (TextInput () + ())
    (User user)
loginForm =
  Uni.build
    $ autheticateBuilder
    <<< ({ email: _, password: _ } <$> emailFormBuilder <*> passwordFormBuilder)
  where
  autheticateBuilder ∷
    ∀ widgets'.
    Uni.Builder
      (Run (AUTH user + eff))
      Msg
      -- (AuthFailed + errs')
      widgets'
      AuthPayload
      (User user)
  autheticateBuilder =
    Uni.sectionValidator
      $ Validator.liftFnMV \r → ado
          res ← Effects.authenticate r.email r.password
          in V (note (Batteries.error _authFailed (const $ "Authentication failed") r) res)

  passwordFormBuilder =
    Password
      <$> Uni.passwordInputBuilder
          { name: "password", policy: identity }

  emailFormBuilder = Uni.emailInputBuilder { name: "email" }
