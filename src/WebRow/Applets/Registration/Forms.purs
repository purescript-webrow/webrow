module WebRow.Applets.Registration.Forms where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Variant (inj)
import Polyform.Reporter (hoistFnEither)
import Polyform.Validator (hoistFnEither) as Validator
import Polyform.Validators.UrlEncoded (string)
import Polyform.Validators.UrlEncoded (string) as Validators
import Type.Prelude (SProxy(..))
import WebRow.Forms.Builders.Plain (passwordField, sectionValidator)
import WebRow.Types (Password(..))

nonEmptyString = string >>> Validator.hoistFnEither \p → case p of
  "" → Left ["Value is required"]
  otherwise → Right p

passwordForm = passwordsForm >>> sectionValidator "no-match" validator
  where
    validator = Validator.hoistFnEither \{ password1, password2 } → if password1 /= password2
      then
        Left ["Password don't match"]
      else
        Right (Password password1)

    passwordsForm = { password1: _, password2: _ }
      <$> passwordField "password1" nonEmptyString
      <*> passwordField "password2" nonEmptyString



--     passwordInput_ = SProxy ∷ SProxy "passwordInput"
--     passwordInput = Widgets.Validator.textInput passwordInput_ Validators.string

-- -- passwordForm ∷ WebRow.Forms.Form.PureValidator (passwordInput ∷ TextInput Identity String) Password
-- passwordForm
--   = passwordsWidget >>> Widget { render, validator }
--   where
--     passwordInput_ = SProxy ∷ SProxy "passwordInput"
--     passwordInput = Widgets.Validator.textInput passwordInput_ Validators.string
-- 
--     render { errors } = inj (SProxy ∷ SProxy "form") (Map.lookup "no-match" errors)
-- 
-- 
