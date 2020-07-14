module WebRow.Applets.Auth.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Prim.Row (class Lacks) as Row
import Record.Builder (Builder, insert) as Record.Builder
import Routing.Duplex (RouteDuplex')
import Routing.Duplex.Generic as DG
import Routing.Duplex.Generic.Syntax ((/))
import Type.Prelude (SProxy(..))
import WebRow.Applets.Auth.Types (Namespace)

data Route
  = Login
  | Logout
  -- | PasswordChange
  --     (Maybe
  --       { oldPassword ∷ Password
  --       , password1 ∷ Password
  --       , password2 ∷ Password
  --       })
derive instance genericRoute ∷ Generic Route _

-- | ( auth ∷ Route | routes)
type RouteRow routes = Namespace Route routes

localDuplex ∷ RouteDuplex' Route
localDuplex = DG.sum
  { "Login": "login" / DG.noArgs
  , "Logout": "logout" / DG.noArgs
  }

routeBuilder
  ∷ ∀ routes
  .  Row.Lacks "auth" routes
  ⇒ Record.Builder.Builder { | routes } { auth ∷ RouteDuplex' Route | routes }
routeBuilder = Record.Builder.insert (SProxy ∷ SProxy "auth") localDuplex
