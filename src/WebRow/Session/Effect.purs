module WebRow.Session.Effect where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run
import Run.Reader (ask)
import Run.State (STATE)
import Run.State as Run.State
import ShopUtils.Logging.Effect as Log
import WebRob.DataStore as DataStore
import WebRow.Session (sessionIdFromRequest, sessionIdKey)

data SessionF session a
  = Get (session → a)
  | Modify (session → session) a
derive instance functorSessionF ∷ Functor (SessionF session)

_session = SProxy ∷ SProxy "session"

type SESSION session = FProxy (SessionF session)

get
  ∷ ∀ session eff
  . Run ( session ∷ SESSION session | eff ) session
get = Run.lift _session (Get identity)

modify
  ∷ ∀ session eff
  . (session → session)
  → Run ( session ∷ SESSION session | eff ) Unit
modify f = Run.lift _session (Modify f unit)


runSession emptySession m = do
  -- extract the session id from cookies, create new in case nothing is found
  sessionId ← sessionIdFromRequest >>= maybe DataStore.createKey pure
  
  -- try to retrieve session data
  sessionBefore ← DataStore.get sessionId >>= flip maybe pure do 
    Log.warning $ "session id key mismatch for " <> sessionId
    pure emptySession

  -- interpret session effect
  Tuple session a ← Run.State.runState sessionBefore
    $ Run.interpret (Run.on _session handleSession Run.send) m
  
  -- save session
  DataStore.set sessionId session

  -- return the result
  pure a

handleSession
  ∷ ∀ a session eff
  . SessionF session a
  → Run ( state ∷ STATE session | eff ) a
handleSession = case _ of
  Get k → k <$> Run.State.get
  Modify f next → Run.State.modify f *> pure next
