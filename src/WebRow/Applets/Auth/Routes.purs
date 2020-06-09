module WebRow.Applets.Auth.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import WebRow.Applets.Auth.Types (Namespace)
import WebRow.Mailer (Email)
import WebRow.Route (printRoute)

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

