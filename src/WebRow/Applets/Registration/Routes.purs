module WebRow.Applets.Registration.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', string)
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as DG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Run (Run)
import WebRow.Applets.Registration.Types (Namespace, SignedEmail, namespace)
import WebRow.Route (FullUrl, ROUTE)
import WebRow.Route (printFullRoute) as Route

data Route
  = RegisterEmail
  | Confirmation SignedEmail
  | ChangeEmail
  | ChangeEmailConfirmation { payload ∷ String }
derive instance genericRoute ∷ Generic Route _

type RouteRow r = Namespace Route r

duplexes ∷ { | Namespace (D.RouteDuplex' Route) () }
duplexes =
  { "register": DG.sum
    { "RegisterEmail": noArgs
    , "Confirmation": "confirmation" / (_Newtype $ D.param "email" ∷ RouteDuplex' SignedEmail)
    , "ChangeEmail": "change-email" / noArgs
    , "ChangeEmailConfirmation": "change-email" / "confirmation" ? { payload: string }
    }
  }

printFullRoute ∷ ∀ eff routes. Route → Run (route ∷ ROUTE (RouteRow routes) | eff) FullUrl
printFullRoute = Route.printFullRoute <<< namespace

