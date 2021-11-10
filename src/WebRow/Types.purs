module WebRow.Types where

import Data.Variant (Variant)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.HTTP (HTTPEXCEPT, REQUEST, COOKIES)
import WebRow.Message (MESSAGE)
import WebRow.Routing (ROUTING')
import WebRow.Session (SESSION)

_webrow = SProxy ∷ SProxy "webrow"

type WebRow messages session routes eff
  = ( COOKIES
        + HTTPEXCEPT
        + MESSAGE (Variant messages)
        + REQUEST
        + ROUTING' routes
        + SESSION session
        + eff
    )
