module Test.WebRow.HTTP where

import Prelude hiding ((/))

import Data.Array (snoc) as A
import Data.Either (Either(..))
import Data.Lazy (force) as Lazy
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Data.Variant (match) as Variant
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (random)
import Foreign.Object (fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..))
import HTTPure (Method(..), Request, fullPath) as HTTPure
import HTTPure.Headers (empty) as Headers
import Routing.Duplex (RouteDuplex', int, parse, print, root, segment, string) as D
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, extract, runBaseAff, runBaseAff', runBaseEffect)
import Run (expand, runBaseAff, runBaseAff', runBaseEffect) as Run
import Run.Reader (runReaderAt)
import Run.State (execStateAt, runStateAt)
import Run.Streaming (Producer, respond) as S
import Run.Streaming (REQUEST)
import Run.Streaming.Prelude (feed, head, take) as S.P
import Run.Streaming.Prelude (fold) as S.Prelude
import Run.Streaming.Pull (chain) as Pull
import Test.Spec (Spec, describe, it, pending)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import WebRow (method, ok) as W
import WebRow.Contrib.Run (AffRow, EffRow)
import WebRow.Crypto (Crypto, _crypto, secret)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP (HTTPExcept, HTTPResponse, Request, SetHeader, Cookies, notFound)
import WebRow.HTTP (fullPath) as HTTP
import WebRow.HTTP.Cookies (defaultAttributes)
import WebRow.HTTP.Cookies (defaultAttributes, lookup, set) as Cookies
import WebRow.HTTP.Request (_request)
import WebRow.HTTP.Response (ok)
import WebRow.Testing.Assertions (shouldEqual)
import WebRow.Testing.HTTP (Client, HTTPSession, _httpSession, get, get_, request)
import WebRow.Testing.HTTP (run, run') as Testing.HTTP
import WebRow.Testing.HTTP.Response (Response(..))

spec :: Spec Unit
spec = do
  pure unit
  describe "WebRow.HTTP" do
    describe "Response" do
      it "SetHeader" do
        let
          client = do
            response ← get "1"
            case response of
              HTTPResponse { parts: { body }} → shouldEqual body "TET"
              otherwise → pure unit
            get_ "2"

          server = do
            path ← HTTP.fullPath
            cs ← Crypto.secret
            c ← Lazy.force <$> Cookies.lookup "test"
            liftEffect $ logShow c
            void $ Cookies.set "test" { value: "test", attributes: Cookies.defaultAttributes }
            r ← liftEffect $ random
            ok $ "TEST" -- (req.url <> ":" <> show r)

        httpSession <- runBaseAff' $ Testing.HTTP.run' {} noArgs pure server client
        logShow $ unsafeStringify httpSession
        pure unit

    --  pending "feature complete"
    -- describe "Features" do
    --   it "runs in NodeJS" $ pure unit
    --   it "runs in the browser" $ pure unit
    --   it "supports streaming reporters" $ pure unit
    --   it "supports async specs" do
    --     res <- delay (Milliseconds 100.0) $> "Alligator"
    --     res `shouldEqual` "Alligator"
    --   it "is PureScript 0.12.x compatible" $ pure unit


