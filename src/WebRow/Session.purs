module WebRow.Session where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run)
import Run (lift) as Run
import Type.Row (type (+))

data SessionF session a
  = Fetch (session → a)
  | Save session

derive instance functorSessionF ∷ Functor (SessionF session)

type SESSION session = FProxy (SessionF session)

_session = SProxy ∷ SProxy "session"

type Session session r = (session ∷ SESSION session | r)

fetch
  ∷ ∀ eff session
  . Run (Session session + eff) session
fetch = Run.lift _session (Fetch identity)

save
  ∷ ∀ a eff session
  . session
  → Run (Session session + eff) a
save session = Run.lift _session (Save session)

modify
  ∷ ∀ eff session
  . (session → session)
  → Run (Session session + eff) Unit
modify m =
  fetch >>= m >>> save


-- sessionIdFromRequest
--   ∷ ∀ eff ctx
--   . Run
--       ( aff ∷ AFF, reader ∷ WebRow.READER ctx | eff )
--       (Maybe UUID)
-- sessionIdFromRequest = map (parseUUID =<< _)
--   $ getCookie' sessionIdKey =<< (ask <#> _.request.headers)
-- 
-- -- cookiesSessionMiddleware
-- --   ∷ ∀ e a r
-- --   . R.Nub ( | SessionAppR r ) ( | SessionAppR r )
-- --   ⇒ (Request → AppMonad_ e (SessionAppR r) a)
-- --   → (Request → AppMonad_ e (BaseAppR r) a)
-- -- cookiesSessionMiddleware router req@{ headers } = do
-- --   -- get session, in case of failure (no valid cookie or sessionId expired) create and Set-Cookie
-- --   { session, resHeaders } ← getSession headers >>= case _ of
-- --     Just session → 
-- --       pure { session, resHeaders: Headers.empty }
-- --     Nothing → do
-- --       { store, secret } ← ask
-- --       session ← liftEffect $ createSession store 
-- --       hv ← liftEffect $ setCookieHeaderSignedValue sessionIdKey session.id defaultCookieAttributes secret
-- --       pure { session, resHeaders: header "Set-Cookie" hv }
-- --   mapExceptT (withReaderT \ctx → Record.disjointUnion
-- --     { session
-- --     , cookies: { sessionId: session.id } 
-- --     , resHeaders
-- --     } ctx) $ router req
-- 
-- -- getSession
-- --   ∷ ∀ eff ctx session
-- --   . Run
-- --       ( aff ∷ AFF
-- --       , reader ∷ WebRow.READER ctx
-- --       , store ∷ SESSIONSTORE session
-- --       | eff
-- --       )
-- --       (Maybe session)
-- -- getSession = sessionIdFromRequest >>= maybe (pure Nothing) DataStore.get
-- 
-- -- createSessionWith 
-- --   ∷ ∀ eff session
-- --   . ({ sessionId ∷ UUID } → session)
-- --   → Run
-- --       ( store ∷ SESSIONSTORE session
-- --       | eff
-- --       )
-- --       session
-- -- createSessionWith mkSession = do
-- --   sessionId ← DataStore.create
-- --   DataStore.set sessionId $ mkSession { sessionId }
-- 
-- type SESSIONSTORE session = STORE UUID session
-- 
-- sessionIdKey ∷ String
-- sessionIdKey = reflectSymbol _sessionId
-- 
-- _sessionId = SProxy ∷ SProxy "sessionId"
