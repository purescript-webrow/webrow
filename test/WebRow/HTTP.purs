module Test.WebRow.HTTP where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Variant (Variant, inj)
import Data.Variant (match) as Variant
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Random (random)
import HTTPure (Method(..))
import HTTPure (Method(..), Request, fullPath) as HTTPure
import HTTPure.Headers (empty) as Headers
import HTTPure.Version (Version(..)) as HTTPure
import Routing.Duplex (RouteDuplex', int, parse, print, root, segment, string) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, liftEffect)
import Run (runBaseAff, runBaseAff', runBaseEffect) as Run
import Run.Reader (runReaderAt)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow (method, ok) as W
import WebRow.Contrib.Run (EffRow, AffRow)
import WebRow.HTTP (HTTPExcept, HTTPResponse, Request, SetHeader, notFound)
import WebRow.HTTP (HTTPResponse) as WebRow.Testing
import WebRow.HTTP.Request (_request)
import WebRow.HTTP.Response.SetHeader (setHeader)
import WebRow.Testing.HTTP.Response (Response(..), run) as Testing.HTTP.Response
import WebRow.Testing.HTTP.Response (Response) as Testing.HTTP

type Route = Variant
  ( "int" :: Int
  , "string" :: String
  )

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes =
      { "int": D.int D.segment
      , "string": D.string D.segment
      }

type Effects = (HTTPExcept + Request + SetHeader + ())

-- app :: Route -> Run (EffRow + Request + SetHeader + ()) (Either String (HTTPResponse String))
router routeDuplex app = \req → do
  let
    testRunner
      -- = Run.runBaseAff'
      = runReaderAt _request req
      <<< Testing.HTTP.Response.run
  testRunner $ D.parse routeDuplex req.url # case _ of
    Right r → Variant.match app r
    Left e → do
      -- traceM "NOT FOUND?"
      -- traceM $ HTTPure.fullPath req
      notFound Headers.empty

string :: String -> Run Effects (HTTPResponse String)
string s = do
  -- x ← show <$> liftEffect random
  let
    x = "TEST"
  W.method >>= case _ of
    Get → do
      setHeader "Set-Cookie" ("x=" <> s)
      W.ok x
    Post → W.ok ("POST" <> x)
    _ → W.ok ("Other" <> x)

app =
  { int: \i → W.ok ("int: " <> show (i ∷ Int))
  , string: string
  }

-- server ∷ HTTPure.Request → Aff (Testing.HTTP.Response String)
server = router routeDuplex app

printUrl = D.print routeDuplex

_string = SProxy ∷ SProxy "string"

get ∷ String → HTTPure.Request
get url =
  { method: HTTPure.Get
  , headers: Headers.empty
  , path: mempty
  , query: mempty
  , body: ""
  , httpVersion: HTTPure.HTTP1_1
  , url
  }


spec :: Spec Unit
spec = do
  describe "WebRow.HTTP" do
    describe "Response" do
      it "SetHeader" do
        let
          url = get (printUrl (inj _string "test"))
        traceM url
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

