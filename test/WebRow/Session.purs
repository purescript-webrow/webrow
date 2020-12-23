module Test.WebRow.Session where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect.Class (liftEffect) as Effect.Class
import Effect.Exception (throw)
import Effect.Ref (new) as Ref
import Polyform.Batteries.Json.Duals (int) as Dual
import Run (Run, liftEffect, runBaseEffect)
import Run (on, run, send) as Run
import Run.Except (catchAt)
import Test.Spec (Spec, describe, it)
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (Secret(..))
import WebRow.HTTP (CookieStore(..), HTTPExcept, SetHeader)
import WebRow.HTTP.Cookies (runOnStore)
import WebRow.HTTP.Response (_httpExcept, _setHeader)
import WebRow.Session (fetch, save) as Session
import WebRow.Session (runInCookieValue, runInMemoryStore)
import WebRow.Session.SessionStore (TTL(..))
import WebRow.Testing.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  let
    runHTTPExcept ∷ ∀ e. Run (EffRow + HTTPExcept + e) ~> Run (EffRow + e)
    runHTTPExcept action = catchAt _httpExcept (const $ liftEffect $ throw $ "TEST") action

    runSetHeader ∷ ∀ e. Run (EffRow + SetHeader + e) Unit → Run (EffRow + e) Unit
    runSetHeader = do
      Run.run
        $ Run.on
            _setHeader
            (const $ pure $ pure unit)
            Run.send
  describe "WebRow.Session" do
    let
      ttl = TTL $ Seconds $ 60.0 * 24.0 * 3.0
    describe "in cookie value" do
      it "performs sudbsquent updates correctly" do
        let
          cookieStore =
            CookieStore
              { requestCookies: mempty
              , secret: Secret "test"
              , responseCookies: mempty
              }

          x =
            runBaseEffect
              $ runSetHeader
              $ runHTTPExcept
              $ runOnStore cookieStore
              $ runInCookieValue (Dual.int) (pure 0) Nothing do
                  value1 ← Session.fetch (Just ttl)
                  Session.save ttl (value1 + 1)
                  value2 ← Session.fetch (Just ttl)
                  (value1 + 1) `shouldEqual` value2
        Effect.Class.liftEffect x
    describe "in memory store" do
      it "performs sudbsquent updates correctly" do
        store ← Effect.Class.liftEffect $ Ref.new mempty
        let
          cookieStore =
            CookieStore
              { requestCookies: mempty
              , secret: Secret "test"
              , responseCookies: mempty
              }

          x =
            runBaseEffect
              $ runSetHeader
              $ runHTTPExcept
              $ runOnStore cookieStore
              $ runInMemoryStore store 0 Nothing do
                  value1 ← Session.fetch (Just ttl)
                  Session.save ttl (value1 + 1)
                  value2 ← Session.fetch (Just ttl)
                  (value1 + 1) `shouldEqual` value2
        Effect.Class.liftEffect x
