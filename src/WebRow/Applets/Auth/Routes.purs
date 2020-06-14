module WebRow.Applets.Auth.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import WebRow.Applets.Auth.Types (Namespace)

data Route
  = Login
  -- | PasswordChange
  --     (Maybe
  --       { oldPassword ∷ Password
  --       , password1 ∷ Password
  --       , password2 ∷ Password
  --       })
derive instance genericRoute ∷ Generic Route _

type RouteRow r = Namespace Route r

duplexes ∷ { | Namespace (D.RouteDuplex' Route) () }
duplexes =
  { "auth": DG.sum
    { "Login": DG.noArgs
    -- , "PasswordChange": D.optional $ D.params
    --     { oldPassword: _Newtype <<< D.string
    --     , password1: _Newtype <<< D.string
    --     , password2: _Newtype <<< D.string
    --     }
    }
  }
