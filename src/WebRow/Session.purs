module WebRow.Session where

import Prelude

import Control.Monad.Except (mapExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, withReaderT)
import Control.Monad.State as State
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty as NonEmpty
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Variant.Internal (FProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTPure (Headers, Request, header, (!@))
import HTTPure as HTTPure
import HTTPure.Headers as Headers
import Prim.Row as R
import Record as Record
import Run (Run(..), AFF)
import Run as Run
import Run.Reader (ask)
import Run.State (State(..))
import Type.Row (type (+))
import WebRow.Cookies (parseCookies)
import WebRow.Crypto (unsign)
import WebRow.Reader (READER)

sessionIdFromRequest
  ∷ ∀ eff ctx
  . Run
      ( aff ∷ AFF, reader ∷ READER ctx | eff )
      (Maybe String)
sessionIdFromRequest = ask <#> _.request <#> _.headers 
  <#> (_ !@ "cookie")
  <#> parseCookies
  <#> hush
  >>= sessionIdFromCookies
  where
    sessionIdFromCookies obj =
      case obj >>= Object.lookup sessionIdKey <#> NonEmpty.head of
        Just signed → hush <$> unsign signed
        Nothing → pure Nothing

-- cookiesSessionMiddleware
--   ∷ ∀ e a r
--   . R.Nub ( | SessionAppR r ) ( | SessionAppR r )
--   ⇒ (Request → AppMonad_ e (SessionAppR r) a)
--   → (Request → AppMonad_ e (BaseAppR r) a)
-- cookiesSessionMiddleware router req@{ headers } = do
--   -- get session, in case of failure (no valid cookie or sessionId expired) create and Set-Cookie
--   { session, resHeaders } ← getSession headers >>= case _ of
--     Just session → 
--       pure { session, resHeaders: Headers.empty }
--     Nothing → do
--       { store, secret } ← ask
--       session ← liftEffect $ createSession store 
--       hv ← liftEffect $ setCookieHeaderSignedValue sessionIdKey session.id defaultCookieAttributes secret
--       pure { session, resHeaders: header "Set-Cookie" hv }
--   mapExceptT (withReaderT \ctx → Record.disjointUnion
--     { session
--     , cookies: { sessionId: session.id } 
--     , resHeaders
--     } ctx) $ router req

-- getSession
--   ∷ ∀ m r
--   . MonadAsk { | RStore + RSecret r } m
--   ⇒ MonadEffect m
--   ⇒ Headers
--   → m (Maybe Session)
-- getSession headers = do
--   { store, secret } ← ask
--   liftEffect $ headers !@ "cookie" # parseCookies # hush # sessionIdFromCookies secret
--     >>= case _ of
--       Nothing → pure Nothing
--       Just c → liftEffect $ store.get c.sessionId

sessionIdKey ∷ String
sessionIdKey = reflectSymbol _sessionId

_sessionId = SProxy ∷ SProxy "sessionId"

-- createSession ∷ MemoryStore Session → Effect Session
-- createSession store = do
--   id ← store.create
--   let session = { id }
--   store.set id session
--   pure session

-- setCookieHeaderSignedValue ∷ Name → Value → CookieAttributes → Secret → Effect String
-- setCookieHeaderSignedValue k v attrs s = do
--   signed ← sign s v
--   pure $ setCookieHeaderValue k signed attrs

