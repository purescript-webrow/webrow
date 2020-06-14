module WebRow.Types where

import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.HTTPError (HttpError)
import WebRow.Mailer (Mailer)
import WebRow.Message (Message)
import WebRow.Request (Request)
import WebRow.Session (Session)

_webrow = SProxy ∷ SProxy "webrow"

type Body = String

type WebRow mails messages session eff =
  ( HttpError
  + Mailer mails
  + Message messages
  + Request
  + Session session
  + eff
  )
