module WebRow.Types where

import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, Request, Cookies)
import WebRow.Message (Message)
import WebRow.Routing (Routing')
import WebRow.Session (Session)

_webrow = SProxy ∷ SProxy "webrow"

type WebRow messages session routes eff
  = ( Cookies
        + HTTPExcept
        + Message messages
        + Request
        + Routing' routes
        + Session session
        + eff
    )
