module WebRow.Testing.HTTP where

import Prelude

import Data.Array (singleton) as Array
import Data.Lazy (defer) as Lazy
import Data.List (List(..), reverse) as List
import Data.List (List)
import Data.Map (fromFoldableWithIndex) as Map
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
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Polyform.Batteries.UrlEncoded.Query (unsafeEncode) as UrlEncoded
import Prim.Row (class Union) as Row
import Routing.Duplex (RouteDuplex')
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
import Type.Row.Homogeneous (class Homogeneous)
import WebRow.Contrib.Run (AffRow, EffRow)
import WebRow.Crypto (Crypto, Secret(..), _crypto)
import WebRow.HTTP (Cookies, HTTPExcept, ResponseCookies, SetHeader)
import WebRow.HTTP.Cookies (_cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (RequestCookies)
import WebRow.HTTP.Request (Request)
import WebRow.Routing (Routing, runRouting)
import WebRow.Session (Session)
import WebRow.Testing.HTTP.Cookies (toRequestCookies)
import WebRow.Testing.HTTP.Response (Render) as Response
import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP
import WebRow.Testing.HTTP.Response (runHTTPExcept, runRender, runSetHeader) as Testing.Response
import WebRow.Testing.HTTP.Types (ClientCookies)
import WebRow.Testing.Session (SessionStoreConfig)
import WebRow.Testing.Session (runInMemory) as Testing.Session

-- | TODO: Upgrdae to polymorphic `body` type here when
-- | monorphic implementation is working.
type Response ctx = Testing.HTTP.Response String ctx

type Exchange ctx =
  { clientCookies ∷ ClientCookies
  , response ∷ Response ctx
  , request ∷ HTTPure.Request
  }

type HTTPSessionConfig = { followRedirects ∷ Boolean }

_httpSession = SProxy ∷ SProxy "httpSession"

type HTTPSession eff = (httpSession ∷ STATE ClientCookies | eff)

-- | During request response exchange
type Client session res eff = S.Client
  { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
  { cookies ∷ ResponseCookies, response ∷ Response res }
  ( S.Producer (Exchange res)
  + HTTPSession
  + Session session
  + eff
  )

request
  ∷ ∀ eff res session
  . HTTPure.Request
  → Run (Client session res + eff) (Response res)
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
  ∷ ∀ eff res session
  . String
  → Run (Client session res + eff) (Response res)
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
  ∷ ∀ eff res session
  . String
  → Run (Client session res + eff) Unit
get_ url = get url *> pure unit

post
  ∷ ∀ eff payload res session
  . Homogeneous payload String
  ⇒ String
  → { | payload }
  → Run (Client session res + eff) (Response res)
post url payload = request
  { method: HTTPure.Method.Post
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: UrlEncoded.unsafeEncode $ Decoded $ Map.fromFoldableWithIndex $ map Array.singleton $ Object.fromHomogeneous $
      payload
  , httpVersion: HTTP1_1
  , url
  }

post_
  ∷ ∀ eff payload res session
  . Homogeneous payload String
  ⇒ String
  → { | payload }
  → Run (Client session res + eff) Unit
post_ url payload = post url payload *> pure unit

type ServerRow session routes res eff =
  ( AffRow
  + Cookies
  + Crypto
  + EffRow
  + HTTPSession
  + HTTPExcept
  + S.Producer (Exchange res)
  + Routing routes
  + Request
  + Session session
  + SetHeader
  + eff
  )

type Server session routes res eff = S.Server
  { cookies ∷ RequestCookies, request ∷ HTTPure.Request }
  { cookies ∷ ResponseCookies, response ∷ Response res }
  (ServerRow session routes res + eff)

type History res = List (Exchange res)

-- | To use this function compiler requires a closed client or server row.
run
  ∷ ∀ eff eff_ res routes session
  . Row.Union eff eff_
      (S.Producer (Exchange res) + eff)
  ⇒ SessionStoreConfig session
  → RouteDuplex' routes
  → Response.Render (Server session routes res + eff) String res
  → Run (Server session routes res + eff) res
  → Run (AffRow + Client session res + EffRow + eff) Unit
  → Run (AffRow + EffRow + eff) (History res)
run sessionStoreConfig routeDuplex render server client = do
  Testing.Session.runInMemory sessionStoreConfig
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
      + S.Producer (Exchange res)
      + S.Producer (Exchange res)
      + Session session
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
            , responseCookies: mempty
            }

        server' { cookies: requestCookies, request: req }
          = server'
          =<< do
            Tuple (CookieStore { responseCookies }) response ← Run.expand $
              runReaderAt _crypto secret
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



runAff
  ∷ ∀ eff eff_ res routes session
  . Row.Union eff eff_
      (S.Producer (Exchange res) + eff)
  ⇒ SessionStoreConfig session
  → RouteDuplex' routes
  → Response.Render (Server session routes res + ()) String res
  → Run (Server session routes res + ()) res
  → Run (AffRow + Client session res + EffRow + ()) Unit
  → Aff (History res)
runAff sessionStore routeDuplex render server client = do
  runBaseAff' $ run sessionStore routeDuplex render server client

run'
  ∷ ∀ eff eff_ res routes session
  . Row.Union eff eff_
      (S.Producer (Exchange res) + eff)
  ⇒ session
  → RouteDuplex' routes
  → Response.Render (Server session routes res + eff) String res
  → Run (Server session routes res + eff) res
  → Run (AffRow + Client session res + EffRow + eff) Unit
  → Run (AffRow + EffRow + eff) (History res)
run' defaultSession routeDuplex render server client = do
  ref ← Run.liftEffect (Effect.Ref.new mempty)
  let
    sc = { ref, default: defaultSession, key: Nothing }
  run sc routeDuplex render server client

runAff'
  ∷ ∀ eff eff_ res routes session
  . Row.Union eff eff_
      (S.Producer (Exchange res) + eff)
  ⇒ session
  → RouteDuplex' routes
  → Response.Render (Server session routes res + ())  String res
  → Run (Server session routes res + ()) res
  → Run (AffRow + Client session res + EffRow + ()) Unit
  → Aff (History res)
runAff' defaultSession routeDuplex render server client = do
  ref ← Effect.liftEffect (Effect.Ref.new mempty)
  let
    sc = { ref, default: defaultSession, key: Nothing }
  runBaseAff' $ run sc routeDuplex render server client
