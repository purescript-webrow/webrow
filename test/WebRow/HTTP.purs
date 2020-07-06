module Test.WebRow.HTTP where

import Prelude hiding ((/))

import Data.Array (snoc) as A
import Data.Either (Either(..))
import Data.Variant (Variant, inj)
import Data.Variant (match) as Variant
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (random)
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..))
import HTTPure (Method(..), Request, fullPath) as HTTPure
import HTTPure.Headers (empty) as Headers
import Routing.Duplex (RouteDuplex', int, parse, print, root, segment, string) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, extract, runBaseAff, runBaseAff', runBaseEffect)
import Run (runBaseAff, runBaseAff', runBaseEffect) as Run
import Run.Reader (runReaderAt)
import Run.State (execStateAt, runStateAt)
import Run.Streaming (Producer, respond) as S
import Run.Streaming (REQUEST)
import Run.Streaming.Prelude (feed, head, take) as S.P
import Run.Streaming.Prelude (fold) as S.Prelude
import Run.Streaming.Pull (chain) as Pull
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow (method, ok) as W
import WebRow.Contrib.Run (EffRow, AffRow)
import WebRow.Crypto (_crypto)
import WebRow.HTTP (HTTPExcept, HTTPResponse, Request, SetHeader, notFound)
import WebRow.HTTP.Request (_request)
import WebRow.Testing.HTTP.HTTPSession (_httpSession)
import WebRow.Testing.HTTP.HTTPSession (x) as HTTPSession
-- import WebRow.HTTP.Response.SetHeader (setHeader)
-- import WebRow.Testing.HTTP.Client (Server, Client)
-- import WebRow.Testing.HTTP.Client (request) as Client
-- import WebRow.Testing.HTTP.Response (Response(..), run) as Testing.HTTP.Response
-- import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP

-- type Route = Variant
--   ( "int" :: Int
--   , "string" :: String
--   )
-- 
-- routeDuplex ∷ D.RouteDuplex' Route
-- routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
--   where
--     routes =
--       { "int": D.int D.segment
--       , "string": D.string D.segment
--       }
-- 
-- -- type Effects = (HTTPExcept + Request + SetHeader + Server String + ())
-- 
-- -- app :: Route -> Run (EffRow + Request + SetHeader + ()) (Either String (HTTPResponse String))
-- router routeDuplex app = \req → do
--   let
--     testRunner
--       -- = Run.runBaseAff'
--       = runReaderAt _request req
--       <<< Testing.HTTP.Response.run
--   testRunner $ D.parse routeDuplex req.url # case _ of
--     Right r → Variant.match app r
--     Left e → do
--       -- traceM "NOT FOUND?"
--       -- traceM $ HTTPure.fullPath req
--       notFound Headers.empty
-- 
-- string :: String -> Run Effects (HTTPResponse String)
-- string s = do
--   -- x ← show <$> Run.liftEffect random
--   let
--     x = "TEST"
--   W.method >>= case _ of
--     Get → do
--       setHeader "Set-Cookie" ("x=" <> s)
--       W.ok x
--     Post → W.ok ("POST" <> x)
--     _ → W.ok ("Other" <> x)
-- 
-- app =
--   { int: \i → W.ok ("int: " <> show (i ∷ Int))
--   , string: string
--   }
-- 
-- -- x = router routeDuplex app
-- 
-- printUrl = D.print routeDuplex
-- 
-- _string = SProxy ∷ SProxy "string"
-- 
-- get ∷ String → HTTPure.Request
-- get url =
--   { method: HTTPure.Get
--   , headers: Headers.empty
--   , path: mempty
--   , query: mempty
--   , body: ""
--   , httpVersion: HTTPure.HTTP1_1
--   , url
--   }
-- 
-- server ∷ HTTPure.Request → Run (Server String + ()) _
-- server req = do
--   res ← (router routeDuplex app) req
--   S.respond res
-- 
-- 
-- client ∷ Run (Client String + ()) _
-- client = do
--   let
--     req = get (printUrl (inj _string "test"))
--   Client.request req

-- toArray ∷ ∀ eff x. Run (S.Producer x eff) Unit → Array x
-- toArray = extract <<< S.Prelude.fold A.snoc [] identity

spec :: Spec Unit
spec = do
  describe "WebRow.HTTP" do
    describe "Response" do
      it "SetHeader" do
         --x = (HTTPSession.x # S.P.feed (S.P.take 4) # toArray))
        httpSession <- runBaseAff' (execStateAt _httpSession mempty (runReaderAt _crypto "SECRET" ((HTTPSession.x) # S.P.head)))
        logShow $ unsafeStringify httpSession
        pure unit

        -- let
        --   url = get (printUrl (inj _string "test"))
        -- let
        --   x = client # Pull.chain server
        -- traceM url
        -- response ← server (get (printUrl (inj _string "test")))
        -- traceM response
        -- case response of
        --   Testing.HTTP.Response.HTTPResponse { headers } →
        --     headers `shouldEqual` Headers.empty
        --   otherwise → pure unit
    --  pending "feature complete"
    -- describe "Features" do
    --   it "runs in NodeJS" $ pure unit
    --   it "runs in the browser" $ pure unit
    --   it "supports streaming reporters" $ pure unit
    --   it "supports async specs" do
    --     res <- delay (Milliseconds 100.0) $> "Alligator"
    --     res `shouldEqual` "Alligator"
    --   it "is PureScript 0.12.x compatible" $ pure unit

