module WebRow.Testing.HTTP.HTTPSession where

import Prelude

import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.JSDate (JSDate)
import Data.Lazy (defer) as Lazy
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (filter, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Effect.Console (logShow)
import Foreign.Object (fromFoldable) as Object
import HTTPure (Method(..)) as HTTPure.Method
import HTTPure (empty) as Headers
import HTTPure.Request (Request) as HTTPure
import HTTPure.Version (Version(..))
import Run (liftEffect)
import Run.Reader (runReaderAt)
import Run.State (STATE, getsAt, modifyAt, runStateAt)
import Run.Streaming (Client, Server, request, respond, yield) as S
import Run.Streaming.Pull (chain) as S.Pull
import Type.Prelude (SProxy(..))
import WebRow.Crypto (_crypto, secret)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP.Cookies (Attributes, Name, Value, _cookies)
import WebRow.HTTP.Cookies.CookieStore (CookieStore(..))
import WebRow.HTTP.Cookies.Types (Values, RequestCookies)
import WebRow.HTTP.Response (ok)
import WebRow.Testing.HTTP.Response (Response) as Testing

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
  eff

runServer :: forall t37 t39 t55 t56 t60 t61 t62.
   t56
   -> t39
      -> t60
         -> Run
              ( await :: FProxy
                           (Step t37
                              (Run
                                 ( await :: FProxy (Step t55 t60)
                                 | t62
                                 )
                                 t55
                              )
                           )
              | t61
              )
              (Tuple t39 t37)
runServer secret cookieStore =
  runStateAt _cookies cookieStore <<< S.request <<< runReaderAt _crypto secret <<< S.request

-- | TODO: Handle cookie expiration.
-- request
--   ∷ ∀ eff
--   . Secret
--   → HTTPure.Request
--   → Run (Client eff) _ -- Response
request :: forall t120 t143 t146.
   t120
   -> Run
        ( await :: FProxy (Step (Tuple CookieStore t143) (Tuple CookieStore t120))
        , crypto :: FProxy (Reader String)
        , httpSession :: FProxy
                           (State
                              { cookies :: Map String
                                             { attributes :: { comment :: Maybe String
                                                             , domain :: Maybe String
                                                             , expires :: Maybe JSDate
                                                             , httpOnly :: Boolean
                                                             , maxAge :: Maybe Int
                                                             , path :: Maybe String
                                                             , sameSite :: Maybe SameSite
                                                             , secure :: Boolean
                                                             }
                                             , value :: String
                                             }
                              , history :: List
                                             { request :: t120
                                             , response :: t143
                                             }
                              }
                           )
        , yield :: FProxy (Step Unit t143)
        | t146
        )
        Unit
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
      { cookies: httpSession.cookies <> responseCookies
      , history: List.Cons { request: req, response } httpSession.history
      }
  modifyAt _httpSession update
  S.yield response

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

x :: forall t190.
   Run
     ( crypto :: FProxy (Reader String)
     , effect :: FProxy Effect
     , httpSession :: FProxy
                        (State
                           { cookies :: Map String
                                          { attributes :: { comment :: Maybe String
                                                          , domain :: Maybe String
                                                          , expires :: Maybe JSDate
                                                          , httpOnly :: Boolean
                                                          , maxAge :: Maybe Int
                                                          , path :: Maybe String
                                                          , sameSite :: Maybe SameSite
                                                          , secure :: Boolean
                                                          }
                                          , value :: String
                                          }
                           , history :: List
                                          { request :: { body :: String
                                                       , headers :: Headers
                                                       , httpVersion :: Version
                                                       , method :: Method
                                                       , path :: Array String
                                                       , query :: Object String
                                                       , url :: String
                                                       }
                                          , response :: HTTPResponse String
                                          }
                           }
                        )
     , yield :: FProxy (Step Unit (HTTPResponse String))
     | t190
     )
     Unit
x = S.Pull.chain server (request $ get "LKJL")
  where
    server (Tuple cookieStore req)
      = server
      =<< do
        r ← runStateAt _cookies cookieStore do
            cs ← secret
            liftEffect $ logShow cs
            (ok "TEST")
        S.respond r

