module WebRow.Applets.Registration.Testing.Templates where

import Prelude
import Global.Unsafe (unsafeStringify)
import Run (Run)
import Text.Smolder.HTML (a, p) as M
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (safe, (!))
import Text.Smolder.Markup (text) as M
import Type.Row (type (+))
import WebRow.Applets.Auth.Routes (Route) as Auth.Routes
import WebRow.Applets.Registration.Responses (ChangeEmailResponse(..), ConfirmationResponse(..), RegisterEmailResponse(..), Response(..))
import WebRow.Applets.Registration.Responses (Response) as Registration
import WebRow.HTTP (HTTPResponse)
import WebRow.HTTP.Response (ok)
import WebRow.Routing (FullUrl(..), Routing')
import WebRow.Testing.Templates (form', html)

render :: forall routes t7. Registration.Response -> Run (Routing' ( auth ∷ Auth.Routes.Route | routes ) + t7) HTTPResponse
render = case _ of
  RegisterEmailResponse r → case r of
    EmailValidationFailed formLayout → ok $ html $ form' formLayout
    EmailSent email (FullUrl url) →
      ok
        $ html do
            M.p
              $ M.text ("Email sent to " <> show email)
            M.p
              $ do
                  M.text ("Activation link is: ")
                  M.a ! (safe $ A.href url) $ M.text url
    InitialEmailForm formLayout → ok $ html $ form' formLayout
  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password →
      ok $ html
        $ do
            M.text
              $ "email: "
              <> unsafeStringify email
              <> "; password: "
              <> unsafeStringify password
    EmailRegisteredInbetween _ → ok $ html $ M.text "Email registered inbetween"
    InitialPasswordForm formLayout → ok $ html $ form' formLayout
    InvalidEmailSignature → ok $ html $ M.text "InvalidEmailSignature"
    PasswordValidationFailed formLayout → ok $ html $ form' formLayout
  ChangeEmailResponse r → case r of
    ChangeEmailInitialForm formLayout → ok $ html $ form' formLayout
