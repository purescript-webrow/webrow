module WebRow.Applets.Auth.Forms where

import Prelude

import Data.Either (note)
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (error) as Batteries
import Polyform.Batteries.String.Validators (NotEmptyExpected)
import Polyform.Batteries.String.Validators (isNotEmpty, NotEmptyExpected) as String
import Polyform.Batteries.UrlEncoded.Validators (SingleValueExpected)
import Polyform.Validator (liftFnMV) as Validator
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, authenticate) as Effects
import WebRow.Applets.Auth.Effects (User)
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Forms (Uni, Layout) as Forms
import WebRow.Forms.Uni (Builder, build, emailInputBuilder, passwordInputBuilder, sectionValidator) as Uni
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Widgets (TextInput)
import WebRow.Mailer (Email)

_authFailed = SProxy ∷ SProxy "authFailed"

type Widgets = (TextInput + ())
type LoginLayout = Forms.Layout Widgets

type AuthPayload = { email ∷ Email, password ∷ Password }
type AuthFailed r = (authFailed ∷ AuthPayload | r)

loginForm :: forall eff errs user.
  Forms.Uni
    (Effects.Auth user + eff)
    (AuthFailed + InvalidEmailFormat + SingleValueExpected + NotEmptyExpected + errs)
    (TextInput + ())
    (User user)
loginForm = Uni.build
  $ autheticateBuilder
  <<< ({ email: _, password: _ } <$> emailFormBuilder <*> passwordFormBuilder)
  where
    autheticateBuilder
      ∷ ∀ errs' widgets'
      . Uni.Builder
          (Effects.Auth user + eff)
          (AuthFailed + String.NotEmptyExpected + errs')
          widgets'
          AuthPayload
          (User user)
    autheticateBuilder = Uni.sectionValidator $ Validator.liftFnMV \r → ado
      res ← Effects.authenticate r.email r.password
      in
        V (note (Batteries.error _authFailed r) res)

    passwordFormBuilder = Password <$> Uni.passwordInputBuilder
      { name: "password", policy: String.isNotEmpty }

    emailFormBuilder = Uni.emailInputBuilder { name: "email" }

