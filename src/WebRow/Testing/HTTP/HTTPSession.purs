module WebRow.Testing.HTTP.HTTPSession where

import Prelude

import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.JSDate (JSDate)
import Data.Lazy (defer) as Lazy
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (filter, toUnfoldable, union, unionWith) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable) as Object
import HTTPure.Request (Request) as HTTPure
import Node.HTTP.Client (responseCookies)
import Node.Simple.Jwt (Secret)
import Run (Run(..))
import Run.Reader (runReaderAt)
import Run.State (STATE, getsAt, modifyAt, runStateAt)
import Run.Streaming (Client, Server, request) as S
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Crypto (Crypto, _crypto, secret)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP.Cookies (Attributes, Name, Value, _cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Headers (requestCookies)
import WebRow.HTTP.Cookies.Types (Values, RequestCookies)
import WebRow.Testing.HTTP.Response (Response) as Testing

type ClientCookieStore = Map Name { value ∷ Value, attributes ∷ Attributes }

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

type Response = Testing.Response String

type History = List { request ∷  HTTPure.Request, response ∷  Response }

type HTTPSessionData =
  { cookies ∷ ClientCookieStore
  , history ∷ History
  }

_httpSession = SProxy ∷ SProxy "httpSession"

type HTTPSession eff = (httpSession ∷ STATE HTTPSessionData | eff)

type Client (eff ∷ # Type) = S.Client
  HTTPure.Request
  Response
  (HTTPSession eff)

type Server (eff ∷ # Type) = S.Server
  HTTPure.Request
  Response
  (HTTPSession eff)

runServer secret cookieStore =
  runStateAt _cookies cookieStore <<< runReaderAt _crypto secret <<< S.request

request
  ∷ ∀ eff
  . Secret
  → HTTPure.Request
  → Run (Client eff) Response
request secret req = do
  requestCookies ←
    getsAt _httpSession _.cookies <#> (toRequestCookies <<< cleanup Nothing)
  let
    cookieStore = CookieStore
      { requestCookies: Lazy.defer \_ → requestCookies, secret, responseCookies: mempty }
  Tuple (CookieStore { responseCookies }) response ← runServer secret cookieStore req
  let
    update httpSession =
      { cookies: httpSession.cookies <> responseCookies
      , history: List.Cons { request: req, response } httpSession.history
      }
  modifyAt _httpSession update
  pure response

