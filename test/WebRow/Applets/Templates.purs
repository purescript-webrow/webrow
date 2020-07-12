module Test.WebRow.Applets.Templates where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, case_, inj, on)
import Run (Run(..))
import Text.Smolder.HTML (Html)
import Text.Smolder.HTML (div, form, h2, html, input) as M
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text) as M
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.String as S
import Type.Row (type (+))
import WebRow.Applets.Auth (Route(..))
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..)) as Auth.Responses
import WebRow.Applets.Auth.Responses (Response) as Auth
import WebRow.Applets.Auth.Routes (Route) as Auth.Routes
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Forms (Layout, LayoutBase(..), TextInput) as Forms
import WebRow.Forms.Widgets (TextInputProps(..), _textInput)
import WebRow.HTTP (HTTPResponse(..))
import WebRow.HTTP.Response (found, ok)
import WebRow.Routing (Routing', fromFullUrl, printFullRoute, printRoute)
import WebRow.Routing.Types (fromRelativeUrl)
import WebRow.Testing.Templates (form, form', html)
import WebRow.Testing.Templates (html) as Testing.Templates

-- toHTTPureResponse ∷ ∀ m. MonadAff m ⇒ Response → m HTTPure.Response
toHTTPResponse :: forall routes t7. Auth.Response -> Run (Routing' (auth ∷ Auth.Routes.Route | routes) + t7) (HTTPResponse String)
toHTTPResponse = case _ of
  Auth.Responses.LoginResponse loginResponse → case loginResponse of
    Auth.Responses.LoginFormValidationFailed formLayout → ok $ html $ form' formLayout
    Auth.Responses.EmailPasswordMismatch formLayout → ok $ html $ form' formLayout
    Auth.Responses.InitialEmailPassordForm formLayout → ok $ html $ form' formLayout
    Auth.Responses.LoginSuccess → ok $ html $ M.text "TEST"
  Auth.Responses.LogoutResponse → do
    redirectTo ← fromFullUrl <$> printFullRoute (inj _auth Login)
    found redirectTo

