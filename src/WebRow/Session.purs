module WebRow.Session where

import Prelude

import Data.Argonaut (Json)
import Data.Either (hush)
import Data.Lazy (force)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Validation.Semigroup (toEither)
import Effect.Ref (Ref)
import HTTPure (empty) as Headers
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Pure
import Run (EFFECT, Run)
import Run (interpret, lift, liftEffect, on, send) as Run
import Type.Row (type (+))
import Type.Prelude (Proxy(..))
import WebRow.Cache (Key)
import WebRow.HTTP (HTTPEXCEPT, internalServerError)
import WebRow.HTTP.Cookies (Attributes, attributes, delete, lookup, lookupJson, set, setJson) as Cookies
import WebRow.HTTP.Cookies (COOKIES)
import WebRow.HTTP.Response.Types (Body(..))
import WebRow.Session.SessionStore (SessionStore, TTL(..))
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (new) as SessionStore.InMemory

data Session session a
  = Delete (Boolean → a)
  -- | When you fetch a given cookie you can extend its TTL.
  | Fetch (Maybe TTL) (session → a)
  | Save TTL session (Boolean → a)

derive instance functorSession ∷ Functor (Session session)

_session = Proxy ∷ Proxy "session"

type SESSION session r
  = ( session ∷ Session session | r )

delete ∷
  ∀ eff session.
  Run (SESSION session +eff) Boolean
delete = Run.lift _session (Delete identity)

modify ∷
  ∀ eff session.
  TTL → (session → session) → Run (HTTPEXCEPT + SESSION session +eff) Unit
modify ttl f = fetch Nothing >>= f >>> save ttl

fetch ∷
  ∀ eff session.
  Maybe TTL →
  Run (SESSION session +eff) session
fetch ttl = Run.lift _session (Fetch ttl identity)

save ∷
  ∀ eff session.
  TTL → session → Run (HTTPEXCEPT + SESSION session +eff) Unit
save ttl session =
  Run.lift _session (Save ttl session identity) >>= not
    >>> if _ then
        -- | TODO:
        -- | * Add loggin
        -- | * Handle this through custom internal exception variant?
        internalServerError Headers.empty $ BodyString "Serious problem on our side..."
      else
        pure unit

cookieName ∷ Key
cookieName = "session"

runInStore ∷
  ∀ eff session.
  SessionStore (Run (COOKIES + EFFECT + eff)) session →
  Maybe (TTL → Cookies.Attributes) →
  Run (COOKIES + EFFECT + SESSION session +eff)
    ~> Run (COOKIES + EFFECT + eff)
runInStore store maybeToCookieAttributes action = do
  let
    toCookieAttributes = case maybeToCookieAttributes of
      Just f → f
      Nothing → \(TTL seconds) → Cookies.attributes _{ maxAge = Just seconds }

    handleSession ∷
      Session session ~> Run (COOKIES + EFFECT + eff)
    handleSession (Delete next) = do
      void $ Cookies.delete cookieName
      store.delete >>= next >>> pure

    handleSession (Fetch maybeTtl next) = do
      -- | TODO:
      -- | * Should we raise here internalServerError when `set` returns `false`?
      -- | * Should we run testing cycle of test cookie setup?
      v ← store.fetch
      case maybeTtl of
        Just ttl → do
          let
            attributes = toCookieAttributes ttl
          void $ store.save ttl v
          void $ Cookies.set cookieName { value: force store.key, attributes }
        Nothing → pure unit
      pure (next v)

    handleSession (Save ttl@(TTL seconds) v next) = do
      let
        attributes = Cookies.attributes _{ maxAge = Just seconds }
      void $ Cookies.set cookieName { value: force store.key, attributes }
      a ← store.save ttl v
      pure (next a)
  Run.interpret (Run.on _session handleSession Run.send) action

-- | Session store useful rather in the testing context.
runInMemoryStore ∷
  ∀ a eff session.
  Ref (Map String session) →
  session →
  Maybe (TTL → Cookies.Attributes) →
  Run (COOKIES + EFFECT + SESSION session +eff) a →
  Run (COOKIES + EFFECT + eff) a
runInMemoryStore ref defaultSession maybeToCookieAttributes action = do
  -- | This laziness is a myth let's drop this all together
  lazySessionKey ← Cookies.lookup cookieName
  effSessionStore ←
    Run.liftEffect
      $ SessionStore.InMemory.new ref defaultSession lazySessionKey
  runInStore (SessionStore.hoist Run.liftEffect $ effSessionStore) maybeToCookieAttributes action

-- | The whole session is stored in a cookie value so visible in the browser.
-- | We don't need any cache here. We could possibly use Cache.Interpret.InCookies
-- | but this seems to only complicate the implementation.
runInCookieValue ∷
  ∀ a eff err session.
  Pure.Dual err Json session →
  Run (COOKIES + EFFECT + eff) session →
  Maybe (TTL → Cookies.Attributes) →
  Run (COOKIES + EFFECT + SESSION session +eff) a →
  Run (COOKIES + EFFECT + eff) a
runInCookieValue dual defaultSession maybeToCookieAttributes =
  let
    toCookieAttributes = case maybeToCookieAttributes of
      Just f → f
      Nothing → \(TTL seconds) → Cookies.attributes _{ maxAge = Just seconds }
    fetchFromCookie = do
      default ← defaultSession
      decode default <<< force <$> Cookies.lookupJson cookieName
      where
      decode default maybeRepr =
        fromMaybe default
          $ (maybeRepr >>= Pure.runValidator dual >>> toEither >>> hush)

    handleSession ∷ Session session ~> Run (COOKIES + EFFECT + eff)
    handleSession (Delete next) = do
      void $ Cookies.delete cookieName
      pure (next true)

    handleSession (Fetch ttl next) = do
      session ← fetchFromCookie
      let
        json = Pure.runSerializer dual session
      -- | TODO:
      -- | * Handle custom cookie attributes (expiration etc.).
      -- | * Should we raise here internalServerError when `set` returns `false`?
      -- | * Should we run testing cycle of test cookie setup?
      case ttl of
        Just t → do
          let
            attributes = toCookieAttributes t
          void $ Cookies.setJson cookieName { json, attributes }
        Nothing → pure unit
      pure $ next session

    handleSession (Save ttl v next) = do
      let
        json = Pure.runSerializer dual v
        attributes = toCookieAttributes ttl
      void $ Cookies.setJson cookieName { json, attributes }
      pure (next true)
  in
    Run.interpret (Run.on _session handleSession Run.send)
