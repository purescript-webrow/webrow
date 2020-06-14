module WebRow.Applets.Registration.Templates.Dummy where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Variant (case_, on)
import Global.Unsafe (unsafeStringify)
import HTTPure (headers, ok') as HTTPure
import Text.Smolder.HTML (Html)
import Text.Smolder.HTML (a, div, form, h2, html, input, p) as M
import Text.Smolder.HTML.Attributes (href, lang, method, name, type', value) as A
import Text.Smolder.Markup (safe, (!))
import Text.Smolder.Markup (text) as M
import Text.Smolder.Renderer.String (render) as S
import Type.Row (type (+))
import WebRow.Applets.Registration.Responses (ChangeEmailResponse(..), ConfirmationResponse(..), RegisterEmailResponse(..), Response(..))
import WebRow.Applets.Registration.Responses (Response)
import WebRow.Applets.Registration.Types (_register)
import WebRow.Forms (Layout) as Forms
import WebRow.Forms.Layout (LayoutBase(..))
import WebRow.Forms.Widgets (TextInput, TextInputProps(..), _textInput)
import WebRow.Route (FullUrl(..))

-- | This is still dummy and unuseful approach
-- | Templates should be separated etc.
onRegister = on _register (render >>> HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ]))

-- | Basic form renderer
html ∷ Html Unit → String
html body = S.render $ M.html ! A.lang "en" $ body

formBody ∷ Forms.Layout (TextInput + ()) → Html Unit
formBody (Section { closed, errors, layout }) = do
  for_ errors \err →
    M.p $ M.text err
  case closed of
    Just { title } → M.h2 $ M.text title
    Nothing → pure unit
  for_ layout formBody
formBody (Widget widget) = c widget
  where
    c = case_
      # on _textInput formField
      -- # on _email formField

formField (TextInputProps { name, payload, result, type_ }) = M.div $ do
  case result of
    Just (Left r) → M.text (fold r)
    otherwise → pure unit
  M.input ! A.type' type_ ! A.name name ! A.value (fromMaybe "" (payload >>= Array.head))

form ∷ Forms.Layout (TextInput + ()) → Html Unit
form l = do
  M.form ! A.method "post" $ do
    formBody l
    M.input ! A.type' "submit" ! A.value "submit"

render ∷ Response (TextInput + ()) → String
render = case _ of
  RegisterEmailResponse r → case r of
    EmailValidationFailed formLayout → html $ form formLayout
    EmailSent email (FullUrl url) → html do
      M.p $
        M.text ("Email sent to " <> show email)
      M.p $ do
        M.text ("Activation link is: ")
        M.a ! (safe $ A.href url) $ M.text url
    InitialEmailForm formLayout → html $ form formLayout

  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
    EmailRegisteredInbetween _ → "Email registered inbetween"
    InitialPasswordForm formLayout → html $ form formLayout
    InvalidEmailSignature → "InvalidEmailSignature"
    PasswordValidationFailed formLayout → html $ form formLayout

  ChangeEmailResponse r → case r of
    ChangeEmailInitialForm formLayout → html $ form formLayout




-- =======
-- import Prelude
-- 
-- import Data.Tuple (Tuple(..))
-- import Data.Variant (on)
-- import Effect.Aff.Class (class MonadAff)
-- import Global.Unsafe (unsafeStringify)
-- import HTTPure as HTTPure
-- import Prim.Row as Row
-- import Run (AFF, Run, liftAff)
-- import Text.Smolder.HTML (a, p) as M
-- import Text.Smolder.HTML.Attributes (href) as A
-- import Text.Smolder.Markup (safe, (!))
-- import Text.Smolder.Markup (text) as M
-- import WebRow.Applets.Auth.Templates.Dummy (form, html)
-- import WebRow.Applets.Registration.Responses (ConfirmationResponse(..), RegisterEmailResponse(..), Response(..))
-- import WebRow.Applets.Registration.Types (_register)
-- import WebRow.Response (RESPONSE, interpretResponseWith, response)
-- import WebRow.Route (FullUrl(..))
-- 
-- -- | This is still dummy and unuseful approach
-- -- | Templates should be separated etc.
-- onRegister = on _register toHTTPureResponse
-- 
-- runResponseRegister ∷ forall eff res.
--   Row.Union eff (response ∷ RESPONSE res) ( response ∷ RESPONSE res | eff )
--   ⇒ Run
--       ( response ∷ RESPONSE ( register ∷ Response | res )
--       , aff ∷ AFF
--       | eff
--       )
--       HTTPure.Response
--   → Run
--       ( response ∷ RESPONSE res
--       , aff ∷ AFF
--       | eff
--       )
--       HTTPure.Response
-- runResponseRegister = interpretResponseWith
--   $ on _register (toHTTPureResponse >>> liftAff) response
-- 
-- toHTTPureResponse ∷ ∀ m. MonadAff m ⇒ Response → m HTTPure.Response
-- toHTTPureResponse = render >>> HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ])
-- 
-- render ∷ Response → String
-- render = case _ of
--   RegisterEmailResponse r → case r of
--     EmailValidationFailed formLayout → html $ form formLayout
--     EmailSent email (FullUrl url) → html do
--       M.p $
--         M.text ("Email sent to " <> show email)
--       M.p $ do
--         M.text ("Activation link is: ")
--         M.a ! (safe $ A.href url) $ M.text url
--     InitialEmailForm formLayout → html $ form formLayout
-- 
--   ConfirmationResponse r → case r of
--     ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
--     EmailRegisteredInbetween _ → "Email registered inbetween"
--     InitialPasswordForm formLayout → html $ form formLayout
--     InvalidEmailSignature → "InvalidEmailSignature"
--     PasswordValidationFailed formLayout → html $ form formLayout
-- 
-- >>>>>>> origin/auth-applet
