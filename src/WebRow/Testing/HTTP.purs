module WebRow.Testing.HTTP where

import Prelude
import Control.Alt ((<|>))
import Data.Array (singleton) as Array
import Data.Lazy (defer) as Lazy
import Data.List (List(..), reverse) as List
import Data.List (List)
import Data.Map (empty, fromFoldableWithIndex) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Effect.Ref (new) as Effect.Ref
import Foreign.Object (fromHomogeneous) as Object
import HTTPure (Method(..)) as HTTPure.Method
import HTTPure (empty) as Headers
import HTTPure.Request (Request) as HTTPure
import HTTPure.Version (Version(..))
import Polyform.Batteries.UrlEncoded (Query(..)) as UrlEncoded
import Polyform.Batteries.UrlEncoded.Query (unsafeEncode) as UrlEncoded
import Prim.Row (class Union) as Row
import Routing.Duplex (RouteDuplex')
import Run (Run, runBaseAff', AFF, EFFECT)
import Run (expand, liftEffect) as Run
import Run.Reader (runReaderAt)
import Run.State (State, evalStateAt, getAt, putAt, runStateAt)
import Run.Streaming (Client, Producer, Server, request, respond, yield) as S
import Run.Streaming.Prelude (fold, take) as S.P
import Run.Streaming.Pull (chain) as S.Pull
import Run.Streaming.Pull (feed) as Pull
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import Type.Row.Homogeneous (class Homogeneous)
import WebRow.Crypto (CRYPTO, Secret(..), _crypto)
import WebRow.HTTP (COOKIES, HTTPEXCEPT, REQUEST, ResponseCookies, SETHEADER)
import WebRow.HTTP.Cookies (_cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (RequestCookies)
import WebRow.Routing (ROUTING, runRouting)
import WebRow.Session (SESSION)
import WebRow.Testing.HTTP.Cookies (toRequestCookies)
import WebRow.Testing.HTTP.Response (Render) as Response
import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP
import WebRow.Testing.HTTP.Response (runHTTPExcept, runRender, runSetHeader) as Testing.Response
import WebRow.Testing.HTTP.Types (ClientCookies)
import WebRow.Testing.Session (SessionStoreConfig)
import WebRow.Testing.Session (runInMemory) as Testing.Session

-- | TODO: Upgrdae to polymorphic `body` type here when
-- | monorphic implementation is working.
type Response ctx
  = Testing.HTTP.Response ctx

type Exchange ctx
  = { clientCookies ∷ ClientCookies
    , response ∷ Response ctx
    , request ∷ HTTPure.Request
    }

type HTTPSessionConfig
  = { followRedirects ∷ Boolean }

_httpSession = SProxy ∷ SProxy "httpSession"

type HTTPSESSION eff
  = ( httpSession ∷ State ClientCookies | eff )

-- | During request response exchange
type Client session res eff
  = S.Client
      { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
      { cookies ∷ ResponseCookies, response ∷ Response res }
      ( S.Producer (Exchange res)
          + HTTPSESSION
          + SESSION session
          + eff
      )

request ∷
  ∀ eff res session.
  HTTPure.Request →
  Run (Client session res + eff) (Response res)
request req = do
  clientCookies ← getAt _httpSession
  let
    -- TODO: Do also `dropExpired now` here
    requestCookies = toRequestCookies clientCookies
  { cookies: responseCookies, response } ←
    S.request
      { cookies: requestCookies, request: req }
  let
    clientCookies' = responseCookies <|> clientCookies
  putAt _httpSession clientCookies'
  S.yield
    { clientCookies: clientCookies'
    , request: req
    , response: response
    }
  pure response

get ∷
  ∀ eff res session.
  String →
  Run (Client session res + eff) (Response res)
get url =
  request
    { method: HTTPure.Method.Get
    , headers: Headers.empty
    , path: mempty
    , query: mempty
    , body: ""
    , httpVersion: HTTP1_1
    , url
    }

get_ ∷
  ∀ eff res session.
  String →
  Run (Client session res + eff) Unit
get_ url = get url *> pure unit

post ∷
  ∀ eff payload res session.
  Homogeneous payload String ⇒
  String →
  { | payload } →
  Run (Client session res + eff) (Response res)
post url payload =
  request
    { method: HTTPure.Method.Post
    , headers: Headers.empty
    , path: mempty
    , query: mempty
    , body:
        UrlEncoded.unsafeEncode $ UrlEncoded.Query $ Map.fromFoldableWithIndex $ map Array.singleton $ Object.fromHomogeneous
          $ payload
    , httpVersion: HTTP1_1
    , url
    }

post_ ∷
  ∀ eff payload res session.
  Homogeneous payload String ⇒
  String →
  { | payload } →
  Run (Client session res + eff) Unit
post_ url payload = post url payload *> pure unit

type ServerRow session routes res eff
  = ( AFF
        + COOKIES
        + CRYPTO
        + EFFECT
        + HTTPSESSION
        + HTTPEXCEPT
        + S.Producer (Exchange res)
        + ROUTING routes
        + REQUEST
        + SESSION session
        + SETHEADER
        + eff
    )

type Server session routes res eff
  = S.Server
      { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
      { cookies ∷ ResponseCookies, response ∷ Response res }
      (ServerRow session routes res + eff)

type History res
  = List (Exchange res)

-- | To use this function compiler requires a closed client or server row.
run ∷
  ∀ eff eff_ res routes session.
  Row.Union
    eff
    eff_
    (S.Producer (Exchange res) + eff) ⇒
  -- (Either (SessionStoreConfig session) (SessionCookieConfig session)) →
  (SessionStoreConfig session) →
  RouteDuplex' routes →
  Response.Render (Server session routes res + eff) res →
  Run (Server session routes res + eff) res →
  Run (AFF + Client session res + EFFECT + eff) Unit →
  Run (AFF + EFFECT + eff) (History res)
run sessionStoreConfig routeDuplex render server client = do
  let
    runSession = Testing.Session.runInMemory sessionStoreConfig
  runSession
    $ evalStateAt _httpSession (Map.empty ∷ ClientCookies)
    $ httpExchange
  where
  -- | This can be a bit unintuitive but we have to expand
  -- | row with another yield so the consumer `take' 100`
  -- | can swallow it.
  httpExchange ∷ Run (AFF + EFFECT + HTTPSESSION + SESSION session + eff) (List (Exchange res))
  httpExchange = go # Pull.feed (S.P.take 100) # S.P.fold (flip List.Cons) List.Nil List.reverse

  -- | I don't really need this signature here but
  -- | maybe it can be a small hint what is going on.
  go ∷
    Run
      ( AFF
          + EFFECT
          + HTTPSESSION
          + S.Producer (Exchange res)
          + S.Producer (Exchange res)
          + SESSION session
          + eff
      )
      Unit
  go = S.Pull.chain server' (Run.expand client)
    where
    secret = Secret "testing-secret"

    cookieStore requestCookies =
      CookieStore
        { requestCookies: Lazy.defer \_ → requestCookies
        , secret
        , responseCookies: Map.empty
        }

    server' { cookies: requestCookies, request: req } =
      server'
        =<< do
            Tuple (CookieStore { responseCookies }) response ←
              Run.expand
                $ runReaderAt _crypto secret
                <<< runStateAt _cookies (cookieStore requestCookies)
                <<< Testing.Response.runSetHeader
                <<< Testing.Response.runHTTPExcept
                <<< runRouting "test.example.com" routeDuplex req
                <<< Testing.Response.runRender render
                $ server
            S.respond
              { cookies: responseCookies
              , response
              }

runAff ∷
  ∀ eff eff_ res routes session.
  Row.Union
    eff
    eff_
    (S.Producer (Exchange res) + eff) ⇒
  SessionStoreConfig session →
  RouteDuplex' routes →
  Response.Render (Server session routes res + ()) res →
  Run (Server session routes res + ()) res →
  Run (AFF + Client session res + EFFECT + ()) Unit →
  Aff (History res)
runAff sessionStore routeDuplex render server client = do
  runBaseAff' $ run sessionStore routeDuplex render server client

run' ∷
  ∀ eff eff_ res routes session.
  Row.Union
    eff
    eff_
    (S.Producer (Exchange res) + eff) ⇒
  session →
  RouteDuplex' routes →
  Response.Render (Server session routes res + eff) res →
  Run (Server session routes res + eff) res →
  Run (AFF + Client session res + EFFECT + eff) Unit →
  Run (AFF + EFFECT + eff) (History res)
run' defaultSession routeDuplex render server client = do
  ref ← Run.liftEffect (Effect.Ref.new Map.empty)
  let
    sc = { ref, default: defaultSession, key: Nothing }
  run sc routeDuplex render server client

runAff' ∷
  ∀ eff eff_ res routes session.
  Row.Union
    eff
    eff_
    (S.Producer (Exchange res) + eff) ⇒
  session →
  RouteDuplex' routes →
  Response.Render (Server session routes res + ()) res →
  Run (Server session routes res + ()) res →
  Run (AFF + Client session res + EFFECT + ()) Unit →
  Aff (History res)
runAff' defaultSession routeDuplex render server client = do
  ref ← Effect.liftEffect (Effect.Ref.new Map.empty)
  let
    sc = { ref, default: defaultSession, key: Nothing }
  runBaseAff' $ run sc routeDuplex render server client
