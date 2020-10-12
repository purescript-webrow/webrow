module WebRow.Session where

import Prelude
import Data.Argonaut (Json)
import Data.Either (hush)
import Data.Lazy (Lazy, defer)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Validation.Semigroup (toEither)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HTTPure (empty) as Headers
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Pure
import Run (FProxy, Run, SProxy(..), liftEffect)
import Run (interpret, lift, liftEffect, on, send) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.HTTP (HTTPExcept, internalServerError)
import WebRow.HTTP.Cookies (Cookies)
import WebRow.HTTP.Cookies (defaultAttributes, delete, lookup, lookupJson, set, setJson) as Cookies
import WebRow.KeyValueStore.Types (Key)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (lazy) as SessionStore.InMemory

data SessionF session a
  = DeleteF (Boolean → a)
  | FetchF (session → a)
  | SaveF session (Boolean → a)

derive instance functorSessionF ∷ Functor (SessionF session)

type SESSION session
  = FProxy (SessionF session)

_session = SProxy ∷ SProxy "session"

type Session session r
  = ( session ∷ SESSION session | r )

delete ∷
  ∀ eff session.
  Run (Session session + eff) Boolean
delete = Run.lift _session (DeleteF identity)

modify ∷
  ∀ eff session.
  (session → session) → Run (HTTPExcept + Session session + eff) Unit
modify f = fetch >>= f >>> save

fetch ∷
  ∀ eff session.
  Run (Session session + eff) session
fetch = Run.lift _session (FetchF identity)

save ∷
  ∀ eff session.
  session → Run (HTTPExcept + Session session + eff) Unit
save session =
  Run.lift _session (SaveF session identity) >>= not
    >>> if _ then
        -- | Collect more request info etc.
        internalServerError Headers.empty "Serious problem on our side..."
      else
        pure unit

cookieName ∷ Key
cookieName = "session"

runInStore ∷
  ∀ eff session.
  Lazy (Effect (SessionStore (Run (Cookies + EffRow + eff)) session)) →
  Run (Cookies + EffRow + Session session + eff)
    ~> Run (Cookies + EffRow + eff)
runInStore store action = Run.interpret (Run.on _session (handleSession store) Run.send) action
  where
  handleSession ∷
    Lazy (Effect (SessionStore (Run (Cookies + EffRow + eff)) session)) →
    SessionF session ~> Run (Cookies + EffRow + eff)
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

runInMemoryStore ∷
  ∀ a eff session.
  Ref (Map String session) →
  session →
  Run (Cookies + EffRow + Session session + eff) a →
  Run (Cookies + EffRow + eff) a
runInMemoryStore ref defaultSession action = do
  lazySessionKey ← Cookies.lookup cookieName
  let
    effSessionStore = SessionStore.InMemory.lazy ref defaultSession lazySessionKey
  runInStore (map (SessionStore.hoist Run.liftEffect) <$> effSessionStore) action

-- | The whole session is stored in a cookie value so visible in the browser.
-- | We don't need any key-value session store.
runInCookieValue ∷
  ∀ a eff err session.
  Pure.Dual err Json session →
  Run (Cookies + EffRow + eff) session →
  Run (Cookies + EffRow + Session session + eff) a →
  Run (Cookies + EffRow + eff) a
runInCookieValue dual defaultSession action = do
  default ← defaultSession
  let
    decode maybeRepr =
      fromMaybe default
        $ (maybeRepr >>= Pure.runValidator dual >>> toEither >>> hush)
  lazySession ← map decode <$> Cookies.lookupJson cookieName
  ref ← liftEffect $ Ref.new lazySession
  Run.interpret (Run.on _session (handleSession ref) Run.send) action
  where
  handleSession ∷
    Ref (Lazy session) →
    SessionF session ~> Run (Cookies + EffRow + eff)
  handleSession ref (DeleteF next) = do
    void $ Cookies.delete cookieName
    default ← defaultSession
    liftEffect $ Ref.write (defer \_ → default) ref
    pure (next true)

  handleSession ref (FetchF next) = do
    lazySession ← liftEffect $ Ref.read ref
    let
      session = Lazy.force lazySession

      json = Pure.runSerializer dual session
    -- | TODO:
    -- | * Handle custom cookie attributes (expiration etc.).
    -- | * Should we raise here internalServerError when `set` returns `false`?
    -- | * Should we run testing cycle of test cookie setup?
    void $ Cookies.setJson cookieName { json, attributes: Cookies.defaultAttributes }
    pure $ next session

  handleSession ref (SaveF v next) = do
    lazySession ← liftEffect $ Ref.read ref
    let
      json = Pure.runSerializer dual v
    void $ Cookies.setJson cookieName { json, attributes: Cookies.defaultAttributes }
    pure (next true)
