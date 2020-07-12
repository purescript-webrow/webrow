module WebRow.Testing.HTTP where

import Prelude

import Data.Lazy (defer) as Lazy
import Data.List (List(..), reverse) as List
import Data.List (List)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Effect.Ref (new) as Effect.Ref
import HTTPure (Method(..)) as HTTPure.Method
import HTTPure (empty) as Headers
import HTTPure.Request (Request) as HTTPure
import HTTPure.Version (Version(..))
import Polyform.Batteries.UrlEncoded.Query (unsafeEncode) as UrlEncoded
import Prim.Row (class Union) as Row
import Run (Run, runBaseAff')
import Run (expand, liftEffect) as Run
import Run.Reader (runReaderAt)
import Run.State (STATE, evalStateAt, getAt, putAt, runStateAt)
import Run.Streaming (Client, Producer, Server, request, respond, yield) as S
import Run.Streaming.Prelude (fold, take) as S.P
import Run.Streaming.Pull (chain) as S.Pull
import Run.Streaming.Pull (feed) as Pull
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow, EffRow)
import WebRow.Crypto (Crypto, _crypto)
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.HTTP (Cookies, HTTPExcept, HTTPResponse, ResponseCookies, SetHeader)
import WebRow.HTTP.Cookies (_cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (RequestCookies)
import WebRow.Session (Session, runSession)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (forRef) as SessionStore.InMemory
import WebRow.Session.SessionStore.Run (SessionStoreRow, _sessionStore)
import WebRow.Testing.HTTP.Cookies (toRequestCookies)
import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP
import WebRow.Testing.HTTP.Response (run) as Testing.Response
import WebRow.Testing.HTTP.Types (ClientCookies)

-- | TODO: Upgrdae to polymorphic `body` type here when
-- | monorphic implementation is working.
type Response = Testing.HTTP.Response String

type Exchange =
  { clientCookies ∷ ClientCookies
  , response ∷ Response
  , request ∷ HTTPure.Request
  }

type HTTPSessionConfig = { followRedirects ∷ Boolean }

_httpSession = SProxy ∷ SProxy "httpSession"

type HTTPSession eff = (httpSession ∷ STATE ClientCookies | eff)

-- | During request response exchange
type Client session eff = S.Client
  { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
  { cookies ∷ ResponseCookies, response ∷ Response }
  ( S.Producer Exchange
  + HTTPSession
  + Session session
  + eff
  )

request
  ∷ ∀ eff session
  . HTTPure.Request
  → Run (Client session + eff) Response
request req = do
  clientCookies ← getAt _httpSession
  let
    -- TODO: Do also `dropExpired now` here
    requestCookies = toRequestCookies clientCookies
  { cookies: responseCookies, response } ← S.request
    { cookies: requestCookies, request: req }
  let
    clientCookies' = responseCookies <> clientCookies
  putAt _httpSession clientCookies'
  S.yield
    { clientCookies: clientCookies'
    , request: req
    , response: response
    }
  pure response

get
  ∷ ∀ eff session
  . String
  → Run (Client session + eff) Response
get url = request
  { method: HTTPure.Method.Get
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: ""
  , httpVersion: HTTP1_1
  , url
  }

get_
  ∷ ∀ eff session
  . String
  → Run (Client session + eff) Unit
get_ url = get url *> pure unit

post
  ∷ ∀ eff session
  . String
  → UrlDecoded
  → Run (Client session + eff) Response
post url decoded = request
  { method: HTTPure.Method.Post
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: UrlEncoded.unsafeEncode decoded
  , httpVersion: HTTP1_1
  , url
  }

post_
  ∷ ∀ eff session
  . String
  → Run (Client session + eff) Unit
post_ url = post_ url *> pure unit

type ServerRow session eff =
  ( AffRow
  + Cookies
  + Crypto
  + EffRow
  + HTTPSession
  + HTTPExcept
  + S.Producer Exchange
  + Session session
  + SetHeader
  + eff
  )

type Server session eff = S.Server
  { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
  { cookies ∷ ResponseCookies, response ∷ Response }
  (ServerRow session + eff)

type History = List Exchange

-- | To use this function compiler requires a closed client or server row.
run
  ∷ ∀ eff eff_ session
  . Row.Union eff eff_
      (S.Producer Exchange + SessionStoreRow (AffRow + EffRow + ()) session + eff)
  ⇒ SessionStore (Run (AffRow + EffRow + ())) session
  → (HTTPure.Request → Run (Server session + eff) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + eff) Unit
  → Run (AffRow + EffRow + eff) History
run sessionStore server client
  -- = runBaseAff'
  = runReaderAt _sessionStore sessionStore
  $ runSession
  $ evalStateAt _httpSession (mempty ∷ ClientCookies)
  $ httpSession
  where
    -- | This can be a bit unintuitive but we have to expand
    -- | row with another yield so the consumer `take' 100`
    -- | can swallow it.
    -- go' = Run.expand go
    httpSession = go # Pull.feed (S.P.take 100) # S.P.fold (flip List.Cons) List.Nil List.reverse

    -- | I don't really need this signature here but
    -- | maybe it can be a small hint what is going on here.
    go ∷ Run
      ( AffRow
      + EffRow
      + HTTPSession
      + S.Producer Exchange
      + S.Producer Exchange
      + SessionStoreRow (AffRow + EffRow + ()) session
      + Session session + eff
      )
      Unit
    go = S.Pull.chain server' (Run.expand client)
      where
        secret = "testing-secret"
        cookieStore requestCookies =
          CookieStore
            { requestCookies: Lazy.defer \_ → requestCookies
            , secret
            , responseCookies: mempty
            }

        server' { cookies: requestCookies, request: req }
          = server'
          =<< do
            Tuple (CookieStore { responseCookies }) response ← Run.expand $
              runReaderAt _crypto secret
              <<< runStateAt _cookies (cookieStore requestCookies)
              <<< Testing.Response.run
              $  server req
            S.respond
              { cookies: responseCookies
              , response
              }

-- | Closed version without extra
runAff
  ∷ ∀ eff eff_ session
  . Row.Union eff eff_
      (S.Producer Exchange + SessionStoreRow (AffRow + EffRow + ()) session + eff)
  ⇒ SessionStore (Run (AffRow + EffRow + ())) session
  → (HTTPure.Request → Run (Server session + ()) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + ()) Unit
  → Aff History
runAff sessionStore server client = do
  runBaseAff' $ run sessionStore server client

run'
  ∷ ∀ eff eff_ session
  . Row.Union eff eff_
      (S.Producer Exchange + SessionStoreRow (AffRow + EffRow + ()) session + eff)
  ⇒ session
  → (HTTPure.Request → Run (Server session + eff) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + eff) Unit
  → Run (AffRow + EffRow + eff) History
run' defaultSession server client = do
  ss ← Effect.liftEffect do
    ref ← Effect.Ref.new mempty
    SessionStore.hoist Run.liftEffect <$>
      SessionStore.InMemory.forRef ref defaultSession
  run ss server client

runAff'
  ∷ ∀ eff eff_ session
  . Row.Union eff eff_
      (S.Producer Exchange + SessionStoreRow (AffRow + EffRow + ()) session + eff)
  ⇒ session
  → (HTTPure.Request → Run (Server session + ()) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + ()) Unit
  → Aff History
runAff' defaultSession server client = do
  ss ← Effect.liftEffect do
    ref ← Effect.Ref.new mempty
    SessionStore.hoist Run.liftEffect <$>
      SessionStore.InMemory.forRef ref defaultSession
  runBaseAff' $ run ss server client
