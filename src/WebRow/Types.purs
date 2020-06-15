module WebRow.Types where

import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept)
import WebRow.Message (Message)
import WebRow.Request (Request)
import WebRow.Route (Route)
import WebRow.Session (Session)

_webrow = SProxy ∷ SProxy "webrow"

type WebRow messages session route eff =
  ( HTTPExcept
  + Message messages
  + Request
  + Route route
  + Session session
  + eff
  )
