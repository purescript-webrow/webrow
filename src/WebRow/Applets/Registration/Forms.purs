module WebRow.Applets.Registration.Forms where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (Pattern(..), contains) as String
import Data.Validation.Semigroup (invalid)
import Polyform.Batteries (error) as Batteries
import Polyform.Validator (check, checkM, liftFn, liftFnV) as Validator
import Polyform.Validator.Dual (check) as Validator.Dual
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Forms.Uni (Layout) as Forms
import WebRow.Forms.Uni (build) as Uni
import WebRow.Forms.Uni (build, emailInputBuilder, passwordInputBuilder, sectionValidator, textInputBuilder) as Forms.Uni
import WebRow.Forms.Widgets (TextInput)
import WebRow.Mailer (Email(..))

-- import WebRow.Forms.Dual ((~))
-- import WebRow.Forms.Dual (Form(..), textInput) as Forms.Dual
-- import WebRow.Forms.Fields.Duals (email) as Fields.Duals
-- import WebRow.Forms.Fields.Validators (email) as Fields.Validators
-- import WebRow.Forms.Plain (input, passwordField, sectionValidator) as Forms.Plain
type Widgets
  = (TextInput + ())

type FormLayout
  = Forms.Layout Widgets

_emailTaken = SProxy ∷ SProxy "emailTaken"

type EmailTaken r
  = ( emailTaken ∷ Email | r )

emailTakenForm = Uni.build $ Forms.Uni.emailInputBuilder { name: "email", policy: validator }
  where
  validator =
    Validator.checkM
      (Batteries.error _emailTaken)
      (map not <$> Effects.emailTaken)

_passwordsMismatch = SProxy ∷ SProxy "passwordsMismatch"

type PasswordsInput
  = { password1 ∷ String, password2 ∷ String }

type PasswordsMismatch r
  = ( passwordsMismatch ∷ PasswordsInput | r )

passwordForm = Forms.Uni.build $ Forms.Uni.sectionValidator validator <<< passwordsForm
  where
  validator =
    Validator.liftFn (Password <<< _.password1)
      <<< Validator.check
          (Batteries.error _passwordsMismatch)
          (\r → r.password1 == r.password2)

  passwordsForm =
    { password1: _, password2: _ }
      <$> Forms.Uni.passwordInputBuilder {}
      <*> Forms.Uni.passwordInputBuilder {}

-- _sameEmail = SProxy ∷ SProxy "sameEmail"
-- 
-- -- | This an example of "Forms.Dual.Form"
-- -- | we are going to drop it from here
-- -- | as bidirectionallity is not necessary
-- -- | in this case (we don't have to fill email value) :-P
-- updateEmailForm email = Forms.Dual.Form $
--   identity ~ Forms.Dual.textInput "" "email" dual
--   <* (const "") ~ Auth.Forms.checkPassword email
--   where
--     emailDiffers = Validator.Dual.check
--       (Batteries.error _sameEmail)
--       (not <<< eq email)
-- 
--     dual = UrlEncoded.Validators.Duals.singleValue >>> Fields.Duals.email >>> emailDiffers >>> Dual.dual' emailTakenValidator
-- 
-- =======
-- import Prelude
-- 
-- import Data.Either (Either(..))
-- import Data.String (Pattern(..), contains) as String
-- import Data.Validation.Semigroup (invalid)
-- import Polyform.Validator (hoistFnEither, hoistFnMV, hoistFnV) as Validator
-- import Polyform.Validator (valid)
-- import Polyform.Validators.UrlEncoded (string)
-- import WebRow.Applets.Auth.Types (Password(..))
-- import WebRow.Applets.Registration.Effects (emailTaken) as Effects
-- import WebRow.Forms.Builders.Plain (field) as Forms.Builders.Plain
-- import WebRow.Forms.Builders.Plain (passwordField, sectionValidator)
-- import WebRow.Mailer (Email(..))
-- 
-- -- | TODO: Move this to polyform validators
-- nonEmptyString = string >>> Validator.hoistFnEither \p → case p of
--   "" → Left ["Value is required"]
--   otherwise → Right p
-- 
-- -- | TODO: Fix this validator - possibly use this:
-- -- |
-- -- | https://github.com/cdepillabout/purescript-email-validate
-- -- |
-- -- | and push this validator to polyform-validators
-- emailFormat = Validator.hoistFnV \email →
--   -- | @ is just enough for as to send an email ;-)
--   if String.contains (String.Pattern "@") email
--     then valid (Email email)
--     else invalid [ "Invalid email format: " <> email ]
-- 
-- emailTaken = Validator.hoistFnMV \email → do
--   Effects.emailTaken email >>= if _
--     then pure $ invalid [ "Email already taken:" <> show email ]
--     else pure $ valid email
-- 
-- emailForm = Forms.Builders.Plain.field { name: "email", type_: "email" } validator
--   where
--     validator = nonEmptyString >>> emailFormat >>> emailTaken
-- 
-- passwordForm = passwordsForm >>> sectionValidator "no-match" validator
--   where
--     validator = Validator.hoistFnEither \{ password1, password2 } → if password1 /= password2
--       then
--         Left ["Passwords don't match"]
--       else
--         Right (Password password1)
-- 
--     passwordsForm = { password1: _, password2: _ }
--       <$> passwordField "password1" nonEmptyString
--       <*> passwordField "password2" nonEmptyString
-- >>>>>>> origin/auth-applet
