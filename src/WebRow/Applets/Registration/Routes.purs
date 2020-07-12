module WebRow.Applets.Registration.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Prim.Row (class Lacks) as Row
import Record.Builder (Builder, insert) as Record.Builder
import Routing.Duplex (RouteDuplex', string)
import Routing.Duplex as D
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as DG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Registration.Types (Namespace, SignedEmail, _registration, namespace)
import WebRow.Routing (FullUrl, Routing')
import WebRow.Routing (printFullRoute) as Route

data Route
  = RegisterEmail
  | Confirmation SignedEmail
  -- | ChangeEmail
  -- | ChangeEmailConfirmation { payload ∷ String }
derive instance genericRoute ∷ Generic Route _

type RouteRow r = Namespace Route r

localDuplex ∷ RouteDuplex' Route
localDuplex = DG.sum
  { "RegisterEmail": noArgs
  , "Confirmation": "confirmation" / (_Newtype $ D.param "email" ∷ RouteDuplex' SignedEmail)
  , "ChangeEmail": "change-email" / noArgs
  , "ChangeEmailConfirmation": "change-email" / "confirmation" ? { payload: string }
  }

routeBuilder
  ∷ ∀ routes
  . Row.Lacks "registration" routes
  ⇒ Record.Builder.Builder { | routes } { registration ∷ RouteDuplex' Route | routes }
routeBuilder = Record.Builder.insert _registration localDuplex

printFullRoute ∷ ∀ eff routes. Route → Run (Routing' (RouteRow routes) + eff) FullUrl
printFullRoute = Route.printFullRoute <<< namespace

