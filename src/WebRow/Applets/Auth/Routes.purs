module WebRow.Applets.Auth.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
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

-- | (route ∷ ( auth ∷ Route | routes) | eff)
type RouteRow routes = (Namespace Route routes)

duplexes ∷ { | Namespace (D.RouteDuplex' Route) () }
duplexes =
  { "auth": DG.sum
    { "Login": DG.noArgs
    , "Logout": DG.noArgs
    -- , "PasswordChange": D.optional $ D.params
    --     { oldPassword: _Newtype <<< D.string
    --     , password1: _Newtype <<< D.string
    --     , password2: _Newtype <<< D.string
    --     }
    }
  }
