module WebRow.Testing.HTTP where

import Prelude

import Data.Lazy (defer) as Lazy
import Data.List (List(..), reverse) as List
import Data.List (List)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect.Class
import HTTPure (Method(..)) as HTTPure.Method
import HTTPure (empty) as Headers
import HTTPure.Request (Request) as HTTPure
import HTTPure.Version (Version(..))
import Run (Run, runBaseAff')
import Run (expand, liftAff) as Run
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
import WebRow.HTTP (Cookies, HTTPExcept, HTTPResponse, ResponseCookies, SetHeader)
import WebRow.HTTP.Cookies (_cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (RequestCookies)
import WebRow.KeyValueStore.InMemory (lifted) as KeyValueStore.InMemory
import WebRow.Session (Session, runSession)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (new) as SessionStore
import WebRow.Session.SessionStore.Run (_sessionStore)
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
  → Run (Client session + eff) Unit
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

get
  ∷ ∀ eff session
  . String
  → Run (Client session + eff) Unit
get url = request
  { method: HTTPure.Method.Get
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: ""
  , httpVersion: HTTP1_1
  , url
  }

type ServerRow session =
  ( AffRow
  + Cookies
  + Crypto
  + EffRow
  + HTTPSession
  + HTTPExcept
  + S.Producer Exchange
  + Session session
  + SetHeader
  + ()
  )

type Server session = S.Server
  { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
  { cookies ∷ ResponseCookies, response ∷ Response }
  (ServerRow session)

type History = List Exchange

run
  ∷ ∀ session
  . SessionStore (Run (AffRow + EffRow + ())) session
  → (HTTPure.Request → Run (Server session) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + ()) Unit
  → Aff History
run sessionStore server client
  = runBaseAff'
  $ runReaderAt _sessionStore sessionStore
  $ runSession
  $ evalStateAt _httpSession (mempty ∷ ClientCookies)
  $ httpSession
  where
    -- | This can be a bit unintuitive but we have to expand
    -- | row with another yield so the consumer `take' 100`
    -- | can swallow it.
    go' = Run.expand go
    httpSession = go' # Pull.feed (S.P.take 100) # S.P.fold (flip List.Cons) List.Nil List.reverse

    go ∷ Run (AffRow + EffRow + HTTPSession + S.Producer Exchange + Session session + ()) Unit
    go = S.Pull.chain server' client
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
            Tuple (CookieStore { responseCookies }) response ←
              runReaderAt _crypto secret
              <<< runStateAt _cookies (cookieStore requestCookies)
              <<< Testing.Response.run
              $  Run.expand (server req)
            S.respond
              { cookies: responseCookies
              , response
              }

run'
  ∷ ∀ session
  . session
  → (HTTPure.Request → Run (Server session) (HTTPResponse String))
  → Run (AffRow + Client session + EffRow + ()) Unit
  → Aff History
run' defaultSession server client = do
  ss ← runBaseAff' do
    kv ← KeyValueStore.InMemory.lifted
      (Run.liftAff <<< Effect.Class.liftEffect)
    SessionStore.new defaultSession kv
  run ss server client


