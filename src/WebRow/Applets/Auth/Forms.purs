module WebRow.Applets.Auth.Forms where

import Prelude

import Data.Either (note)
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (error) as Batteries
import Polyform.Batteries.UrlEncoded.Validators (SingleValueExpected)
import Polyform.Batteries.UrlEncoded.Validators (singleValue) as Batteries
import Polyform.Validator (liftFnMV) as Validator
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, authenticate) as Effects
import WebRow.Applets.Auth.Effects (User)
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Forms (Uni) as Forms
import WebRow.Forms.Uni (Builder, build, passwordInputBuilder, sectionValidator, textInputBuilder) as Uni
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Validators (email) as Validators
import WebRow.Forms.Widgets (TextInput)
import WebRow.Mailer (Email)

_authFailed = SProxy ∷ SProxy "authFailed"

type AuthInput = { email ∷ Email, password ∷ Password }
type AuthFailed r = (authFailed ∷ AuthInput | r)

loginForm :: forall eff errs user widgets.
  Forms.Uni
    ( Effects.Auth user + eff)
    ( AuthFailed + InvalidEmailFormat + SingleValueExpected + errs)
    ( TextInput + widgets)
    (User user)
loginForm = Uni.build
  $ autheticateBuilder
  <<< ({ email: _, password: _ } <$> emailFormBuilder <*> passwordFormBuilder)
  where
    autheticateBuilder
      ∷ ∀ errs' widgets'
      . Uni.Builder
          (Effects.Auth user + eff)
          ( AuthFailed + errs' )
          widgets'
          AuthInput
          (User user)
    autheticateBuilder = Uni.sectionValidator $ Validator.liftFnMV \r → ado
      res ← Effects.authenticate r.email r.password
      in
        V (note (Batteries.error _authFailed r) res)

    passwordFormBuilder = Password <$> Uni.passwordInputBuilder {}

    emailFormBuilder = Uni.textInputBuilder
      { type_: "email"
      , validator: Batteries.singleValue >>> Validators.email
      }


-- -- -- | This is bidirectional form which serializer is not isomoprhic on purpose ;-)
-- -- -- passwordConfirmation email =
-- -- -- _password = SProxy ∷ SProxy "password"
-- -- 
-- -- -- passwordFormBuilder email = Forms.Bi.textInputBuilder "" "password" dual
-- -- --   where
-- -- --     dual = Dual.dual validator serializer
-- -- --     -- | We don't want to prefill password field ;-)
-- -- --     serializer = const $ pure (Just [""])
-- -- --     validator = UrlEncoded.Validators.singleValue >>> String.Validators.isNotEmpty >>> Validator.checkM
-- -- --         (Batteries.error _password)
-- -- --         (Effects.checkPassword email <<< Password)
--
-- import WebRow.Applets.Registration.Forms (emailFormat, nonEmptyString)
-- import WebRow.Forms.Builders.Plain (passwordField)
-- import WebRow.Forms.Builders.Plain as Forms.Builders.Plain
-- 
-- emailPassordForm = { email: _, password: _ } <$> emailForm <*> passwordForm
--   where
--   passwordForm = passwordField "password" nonEmptyString
--   emailForm = Forms.Builders.Plain.field
--     { name: "email", type_: "email" }
--     (nonEmptyString >>> emailFormat)
-- >>>>>>> origin/auth-applet
