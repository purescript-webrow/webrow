module WebRow.Session.Effect where

import Prelude

import Data.Maybe (maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Data.Variant.Internal (FProxy)
import Prim.Row as Row
import Run (Run, AFF)
import Run as Run
import Run.State (STATE)
import Run.State as Run.State
import WebRow.Cookies as Cookies
import WebRow.DataStore as DataStore
import WebRow.Logging.Effect (LOGGER)
import WebRow.Logging.Effect as Log
import WebRow.Reader as WebRow
import WebRow.Response.Modify (MODIFY)
import WebRow.Response.Modify as Response.Modify
import WebRow.Session (SESSIONSTORE, sessionIdFromRequest, sessionIdKey)

get
  ∷ ∀ session eff
  . Run ( session ∷ SESSION session | eff ) session
get = Run.lift _session (Get identity)

modify
  ∷ ∀ session eff
  . (session → session)
  → Run ( session ∷ SESSION session | eff ) Unit
modify f = Run.lift _session (Modify f unit)

set
  ∷ ∀ session eff
  . session
  → Run ( session ∷ SESSION session | eff ) Unit
set session = modify $ const session

type Effects session ctx eff =
  ( aff ∷ AFF
  , logger ∷ LOGGER
  , modifyResponse ∷ MODIFY
  , reader ∷ WebRow.READER ctx
  , store ∷ SESSIONSTORE session
  | eff
  )

withSession
  ∷ ∀ session ctx eff a
  . Row.Union eff ( state ∷ STATE session ) ( state ∷ STATE session | eff )
  ⇒ Discard session
  ⇒ Run (                           | Effects session ctx eff ) session
  → Run ( session ∷ SESSION session | Effects session ctx eff ) a
  → Run (                           | Effects session ctx eff ) a
withSession onEmptySession m = do
  -- extract the session id from cookies, create new in case nothing is found
  sessionId ← sessionIdFromRequest >>= maybe DataStore.create pure

  -- try to retrieve session data
  sessionBefore ← DataStore.get sessionId >>= flip maybe pure do 
    Log.warning $ "session id key mismatch for " <> UUID.toString sessionId
    onEmptySession

  -- interpret session effect
  Tuple session a ← Run.State.runState sessionBefore $ interpretSessionToState m

  -- save session
  DataStore.set sessionId session
  Response.Modify.setCookie
    { name: sessionIdKey
    , value: UUID.toString sessionId
    , attributes: Cookies.defaultAttributes
    }

  -- return the result
  pure a

interpretSessionToState
  ∷ ∀ a session eff
  . Row.Union eff ( state ∷ STATE session ) ( state ∷ STATE session | eff )
  ⇒ Run ( session ∷ SESSION session | eff ) a
  → Run ( state   ∷ STATE   session | eff ) a
interpretSessionToState = Run.interpret
  (Run.on _session handleSession (Run.send >>> Run.expand))

handleSession
  ∷ ∀ a session eff
  . SessionF session a
  → Run ( state ∷ STATE session | eff ) a
handleSession = case _ of
  Get k → k <$> Run.State.get
  Modify f next → Run.State.modify f *> pure next

data SessionF session a
  = Get (session → a)
  | Modify (session → session) a
derive instance functorSessionF ∷ Functor (SessionF session)

_session = SProxy ∷ SProxy "session"

type SESSION session = FProxy (SessionF session)
