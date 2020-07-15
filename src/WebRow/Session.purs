module WebRow.Session where

import Prelude

import Data.Lazy (Lazy)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Effect (Effect)
import Effect.Ref (Ref)
import HTTPure (empty) as Headers
import Run (FProxy, Run, SProxy(..))
import Run (interpret, lift, liftEffect, on, send) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.HTTP (HTTPExcept, internalServerError)
import WebRow.HTTP.Cookies (Cookies)
import WebRow.HTTP.Cookies (defaultAttributes, delete, lookup, set) as Cookies
import WebRow.KeyValueStore.Types (Key)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (lazy) as SessionStore.InMemory

data SessionF session a
  = DeleteF (Boolean → a)
  | FetchF (session → a)
  | SaveF session (Boolean → a)

derive instance functorSessionF ∷ Functor (SessionF session)

type SESSION session = FProxy (SessionF session)

_session = SProxy ∷ SProxy "session"

type Session session r = (session ∷ SESSION session | r)

delete
  ∷ ∀ eff session
  . Run (Session session + eff) Boolean
delete = Run.lift _session (DeleteF identity)

modify
  ∷ ∀ eff session
  . (session → session) → Run (HTTPExcept + Session session + eff) Unit
modify f = fetch >>= f >>> save

fetch
  ∷ ∀ eff session
  . Run (Session session + eff) session
fetch = Run.lift _session (FetchF identity)

save
  ∷ ∀ eff session
  . session → Run (HTTPExcept + Session session + eff) Unit
save session = Run.lift _session (SaveF session identity) >>= not >>> if _
  then
    -- | Collect more request info etc.
    internalServerError Headers.empty "Serious problem on our side..."
  else
    pure unit

cookieName ∷ Key
cookieName = "sessionId"

handleSession
  ∷ ∀ eff session
  . Lazy (Effect (SessionStore (Run (Cookies + EffRow + eff)) session))
  → SessionF session ~> Run (Cookies + EffRow + eff)
handleSession ss (DeleteF next) = do
  void $ Cookies.delete cookieName
  Run.liftEffect (Lazy.force ss) >>= _.delete >>= next >>> pure
handleSession ss (FetchF next) = do
  ss' ← Run.liftEffect $ Lazy.force ss
  -- | TODO:
  -- | * Handle custom cookie attributes (expiration etc.).
  -- | * Should we raise here internalServerError when `set` returns `false`?
  -- | * Should we run testing cycle of test cookie setup?
  void $ Cookies.set cookieName { value: ss'.key, attributes: Cookies.defaultAttributes }
  ss'.fetch >>= next >>> pure
handleSession ss (SaveF v next) = do
  ss' ← Run.liftEffect $ Lazy.force ss
  void $ Cookies.set cookieName { value: ss'.key, attributes: Cookies.defaultAttributes }
  ss'.save v >>= next >>> pure

run ∷ ∀ eff session
  . Lazy (Effect (SessionStore (Run (Cookies + EffRow + eff)) session))
  → Run (Cookies + EffRow + Session session + eff)
  ~> Run (Cookies + EffRow + eff)
run ss action = Run.interpret (Run.on _session (handleSession ss) Run.send) action

runInMemory ∷ ∀ a eff session
  . Ref (Map String session)
  → session
  → Run (Cookies + EffRow + Session session + eff) a
  → Run (Cookies + EffRow + eff) a
runInMemory ref defaultSession action = do
  lazySessionKey ← Cookies.lookup cookieName
  let
    effSessionStore = SessionStore.InMemory.lazy ref defaultSession lazySessionKey
  run (map (SessionStore.hoist Run.liftEffect) <$> effSessionStore) action

