module WebRow.Testing.HTTP.HTTPSession where

import Prelude

import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.JSDate (JSDate)
import Data.Lazy (defer, force) as Lazy
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (filter, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Effect.Console (logShow)
import Effect.Random (random)
import Foreign.Object (fromFoldable) as Object
import HTTPure (Method(..)) as HTTPure.Method
import HTTPure (empty) as Headers
import HTTPure.Request (Request) as HTTPure
import HTTPure.Version (Version(..))
import Run (Run, liftEffect)
import Run.State (STATE, getsAt, modifyAt, runStateAt)
import Run.Streaming (Client, Server, Producer, request, respond, yield) as S
import Run.Streaming (YIELD)
import Run.Streaming.Pull (chain) as S.Pull
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Crypto (Crypto, secret)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP.Cookies (Attributes, Name, Value, _cookies)
import WebRow.HTTP.Cookies (defaultAttributes, lookup, set) as Cookies
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (Values, RequestCookies)
import WebRow.HTTP.Response (ok)
import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP
import WebRow.Testing.HTTP.Response (run) as Testing.Response

type ClientCookieStore = Map Name { value ∷ Value, attributes ∷ Attributes }

-- | We should probably abstract away expiration here
-- | by just taking filtering function for cookies.
cleanup ∷ Maybe JSDate → ClientCookieStore → ClientCookieStore
cleanup Nothing = identity
cleanup (Just now) = Map.filter valid
  where
    valid { attributes: { expires }}  = fromMaybe false ((now < _) <$> expires)

toRequestCookies ∷ ClientCookieStore -> RequestCookies
toRequestCookies =
  let
    arr ∷ ClientCookieStore → Array (Tuple String Values)
    arr = map (identity *** _.value >>> Array.NonEmpty.singleton) <<< Map.toUnfoldable
  in
    Object.fromFoldable <<< arr

type Response = Testing.HTTP.Response String

type History = List { request ∷  HTTPure.Request, response ∷  Response }

type HTTPSessionData =
  { cookies ∷ ClientCookieStore
  -- | A full history including redirects
  , history ∷ History
  }

_httpSession = SProxy ∷ SProxy "httpSession"

type HTTPSession eff = (httpSession ∷ STATE HTTPSessionData | eff)

type Exchange = { response ∷ Response, request ∷ HTTPure.Request }
type Client (eff ∷ # Type) = S.Client
  (Tuple CookieStore HTTPure.Request)
  (Tuple CookieStore Response)
  (S.Producer Exchange + HTTPSession + eff)

type Server (eff ∷ # Type) = S.Server
  HTTPure.Request
  Response
  eff


request
  ∷ ∀ eff
  . HTTPure.Request
  → Run (Crypto + Client + eff) Unit
request req = do
  requestCookies ←
    getsAt _httpSession _.cookies <#> (toRequestCookies <<< cleanup Nothing)
  secret ← Crypto.secret
  let
    cookieStore = CookieStore
      { requestCookies: Lazy.defer \_ → requestCookies, secret, responseCookies: mempty }
  Tuple (CookieStore { responseCookies }) response ← S.request (Tuple cookieStore req)
  let
    update httpSession =
      { cookies: responseCookies <> httpSession.cookies
      , history: List.Cons { request: req, response } httpSession.history
      }
  modifyAt _httpSession update
  S.yield { request: req, response: response }

get ∷ String → HTTPure.Request
get url =
  { method: HTTPure.Method.Get
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: ""
  , httpVersion: HTTP1_1
  , url
  }

x ∷ _
x server = S.Pull.chain server' (request $ get "LKJL")
  where
    server' (Tuple cookieStore req)
      = server'
      =<< do
        r ← runStateAt _cookies cookieStore <<< Testing.Response.run $ server
        S.respond r

