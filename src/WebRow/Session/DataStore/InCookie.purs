module WebRow.Session.DataStore.InCookie where

import Prelude

import Data.Maybe (Maybe(..), isJust, maybe)
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Run (AFF, Run)
import Run as Run
import Run.Reader (ask)
import WebRow.Cookies (getCookie')
import WebRow.Cookies as Cookies
import WebRow.DataStore (DataStoreF(..), _store)
import WebRow.Logging.Effect (LOGGER)
import WebRow.Logging.Effect as Log
import WebRow.Reader as WebRow
import WebRow.Response.Modify (MODIFY)
import WebRow.Response.Modify as Response.Modify
import WebRow.Session (SESSIONSTORE, sessionIdFromRequest)

sessionKey ∷ String
sessionKey = "session"

type Effects ctx eff =
  ( aff ∷ AFF
  , logger ∷ LOGGER
  , reader ∷ WebRow.READER ctx
  , modifyResponse ∷ MODIFY
  | eff
  )

runSessionStoreInCookies
  ∷ ∀ ctx a session eff
  . (String → Run ( Effects ctx eff ) (Maybe session))
  → (session → String)
  → session
  → Run ( store ∷ SESSIONSTORE session | Effects ctx eff ) a
  → Run ( Effects ctx eff ) a
runSessionStoreInCookies decodeSession encodeSession newSession m = do
  -- retrieve the current session state
  session ← ask <#> _.request.headers
    >>= getCookie' sessionKey
    >>= maybe (pure Nothing) decodeSession
  sessionId ← sessionIdFromRequest
  ref ← lift $ Ref.new $ { sessionId: _, session: _ } <$> sessionId <*> session
  
  -- interpret 'store' effect in `m` 
  m' ← Run.interpret
    (Run.on _store (singleValueStoreHandlerWithCookie ref newSession) Run.send) m

  -- save session
  lift (Ref.read ref) >>= case _ of
    Nothing → do
      Response.Modify.deleteCookie sessionKey
    Just r → do
      Response.Modify.setCookie
        { name: sessionKey
        , value: encodeSession r.session
        , attributes: Cookies.defaultAttributes
        }

  -- return the rest of the program
  pure m'

singleValueStoreHandlerWithCookie
  ∷ ∀ session eff a
  . Ref (Maybe { sessionId ∷ UUID, session ∷ session })
  → session
  → DataStoreF UUID session a
  → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) a
singleValueStoreHandlerWithCookie ref newSession = case _ of
  Create onKey → do
    s ← lift $ Ref.read ref
    when (isJust s) $ Log.warning "DataStore.create overrides already created sessionId"
    sessionId ← lift genUUID
    lift $ Ref.modify_ (const $ Just { sessionId, session: newSession}) ref
    pure $ onKey sessionId
  Delete key next → do
    -- logKeyMismatch key "DataStore.delete"
    lift $ Ref.write Nothing ref
    pure next
  Get key onMaybeVal → do
    -- logKeyMismatch key "DataStore.get"
    onMaybeVal <$> map _.session <$> lift (Ref.read ref)
  Set key val onVal → do
    -- logKeyMismatch key "DataStore.set"
    lift $ Ref.write (Just { sessionId: key, session: val }) ref
    pure $ onVal val

lift ∷ forall eff a. Effect a → Run ( aff ∷ AFF | eff ) a
lift = Run.liftAff <<< liftEffect

    -- logKeyMismatch key msg = do
    --   lift (Ref.read ref) >>= maybe (pure unit) \s → do
    --     when (s.sessionId /= key) $ Log.warning $ msg <> ": sessionId mismatch"
    --     lift $ Ref.write Nothing ref