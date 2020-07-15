module WebRow.Applets.Auth.Testing.Templates where

import Prelude

import Data.Variant (inj)
import Run (Run)
import Text.Smolder.Markup (text) as M
import Type.Row (type (+))
import WebRow.Applets.Auth (Route(..))
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..)) as Auth.Responses
import WebRow.Applets.Auth.Responses (Response) as Auth
import WebRow.Applets.Auth.Routes (Route) as Auth.Routes
import WebRow.Applets.Auth.Types (_auth)
import WebRow.HTTP (HTTPResponse)
import WebRow.HTTP.Response (found, ok)
import WebRow.Routing (Routing', fromRelativeUrl, printRoute)
import WebRow.Testing.Templates (form', html)

render :: forall routes t7. Auth.Response -> Run (Routing' (auth ∷ Auth.Routes.Route | routes) + t7) (HTTPResponse String)
render = case _ of
  Auth.Responses.LoginResponse loginResponse → case loginResponse of
    Auth.Responses.LoginFormValidationFailed formLayout → ok $ html $ form' formLayout
    Auth.Responses.EmailPasswordMismatch formLayout → ok $ html $ form' formLayout
    Auth.Responses.InitialEmailPassordForm formLayout → ok $ html $ form' formLayout
    Auth.Responses.LoginSuccess → ok $ html $ M.text "TEST"
  Auth.Responses.LogoutResponse → do
    redirectTo ← fromRelativeUrl <$> printRoute (inj _auth Login)
    found redirectTo

