module Test.WebRow.HTTP where

import Prelude hiding ((/))
import Data.Lazy (force) as Lazy
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (random)
import Global.Unsafe (unsafeStringify)
import Routing.Duplex.Generic (noArgs)
import Run (runBaseAff')
import Test.Spec (Spec, describe, it)
import WebRow.Crypto (secret) as Crypto
import WebRow.HTTP (fullPath) as HTTP
import WebRow.HTTP.Cookies (defaultAttributes, lookup, set) as Cookies
import WebRow.HTTP.Response (ok)
import WebRow.Session (fetch) as Session
import WebRow.Testing.Assertions (shouldEqual)
import WebRow.Testing.HTTP (get, get_)
import WebRow.Testing.HTTP (run') as Testing.HTTP
import WebRow.Testing.HTTP.Response (bodyString) as Response

spec :: Spec Unit
spec = do
  describe "WebRow.HTTP" do
    describe "Response" do
      it "SetHeader" do
        let
          client = do
            response ← get "1"
            Response.bodyString response `shouldEqual` Just "TEST"
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
  describe "WebRow.Session" do
    describe "In cookie handling" do
      it "Should handle multiple cookie modifications" do
        let
          client = do
            response ← get "1"
            Response.bodyString response `shouldEqual` Just "TEST"
            get_ "2"

          server = do
            value ← Session.fetch Nothing
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
