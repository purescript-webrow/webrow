module WebRow.Session where

import Prelude

import Data.Argonaut (Json)
import Data.Either (hush)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Validation.Semigroup (toEither)
import Effect (Effect)
import Effect.Ref (Ref)
import HTTPure (empty) as Headers
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Pure
import Run (FProxy, Run, SProxy(..))
import Run (interpret, lift, liftEffect, on, send) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.HTTP (HTTPExcept, internalServerError)
import WebRow.HTTP.Cookies (Cookies)
import WebRow.HTTP.Cookies (defaultAttributes, delete, lookup, lookupJson, set, setJson) as Cookies
import WebRow.KeyValueStore.Types (Key)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (new) as SessionStore.InMemory

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
  Effect (SessionStore (Run (Cookies + EffRow + eff)) session) →
  Run (Cookies + EffRow + Session session + eff)
    ~> Run (Cookies + EffRow + eff)
runInStore store = runInRunStore (Run.liftEffect store)

runInRunStore ∷
  ∀ eff session.
  Run (Cookies + EffRow + eff) (SessionStore (Run (Cookies + EffRow + eff)) session) →
  Run (Cookies + EffRow + Session session + eff)
    ~> Run (Cookies + EffRow + eff)
runInRunStore store action = do
  s ← store
  let
    handleSession ∷
      SessionF session ~> Run (Cookies + EffRow + eff)
    handleSession (DeleteF next) = do
      void $ Cookies.delete cookieName
      s.delete >>= next >>> pure

    handleSession (FetchF next) = do
      -- | TODO:
      -- | * Handle custom cookie attributes (expiration etc.).
      -- | * Should we raise here internalServerError when `set` returns `false`?
      -- | * Should we run testing cycle of test cookie setup?
      void $ Cookies.set cookieName { value: s.key, attributes: Cookies.defaultAttributes }
      s.fetch >>= next >>> pure

    handleSession (SaveF v next) = do
      void $ Cookies.set cookieName { value: s.key, attributes: Cookies.defaultAttributes }
      a ← s.save v
      pure (next a)

  Run.interpret (Run.on _session handleSession Run.send) action

runInMemoryStore ∷
  ∀ a eff session.
  Ref (Map String session) →
  session →
  Run (Cookies + EffRow + Session session + eff) a →
  Run (Cookies + EffRow + eff) a
runInMemoryStore ref defaultSession action = do
  -- | This laziness is a myth let's drop this all together
  lazySessionKey ← Cookies.lookup cookieName
  let
    effSessionStore = SessionStore.InMemory.new ref defaultSession (Lazy.force lazySessionKey)
  runInStore (SessionStore.hoist Run.liftEffect <$> effSessionStore) action

-- | The whole session is stored in a cookie value so visible in the browser.
-- | We don't need any key-value session store.
runInCookieValue ∷
  ∀ a eff err session.
  Pure.Dual err Json session →
  Run (Cookies + EffRow + eff) session →
  Run (Cookies + EffRow + Session session + eff) a →
  Run (Cookies + EffRow + eff) a
runInCookieValue dual defaultSession =
  let
    fetchFromCookie = do
      default ← defaultSession
      map (decode default) <$> Cookies.lookupJson cookieName
      where
        decode default maybeRepr =
          fromMaybe default
            $ (maybeRepr >>= Pure.runValidator dual >>> toEither >>> hush)

    handleSession ∷ SessionF session ~> Run (Cookies + EffRow + eff)
    handleSession (DeleteF next) = do
      void $ Cookies.delete cookieName
      pure (next true)

    handleSession (FetchF next) = do
      session ← Lazy.force <$> fetchFromCookie
      let
        json = Pure.runSerializer dual session
      -- | TODO:
      -- | * Handle custom cookie attributes (expiration etc.).
      -- | * Should we raise here internalServerError when `set` returns `false`?
      -- | * Should we run testing cycle of test cookie setup?
      void $ Cookies.setJson cookieName { json, attributes: Cookies.defaultAttributes }
      pure $ next session

    handleSession (SaveF v next) = do
      lazySession ← fetchFromCookie
      let
        json = Pure.runSerializer dual v
      void $ Cookies.setJson cookieName { json, attributes: Cookies.defaultAttributes }
      pure (next true)
  in
    Run.interpret (Run.on _session handleSession Run.send)

