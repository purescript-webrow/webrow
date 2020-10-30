module WebRow.Testing.HTTP.Response where

import Prelude

import Data.Maybe (Maybe(..))
import HTTPure (Headers, header) as HTTPure
import HTTPure (Status)
import HTTPure.Headers (empty) as HTTPure.Headers
import Run (Run)
import Run (on, run, send) as Run
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept)
import WebRow.HTTP (HTTPException) as HTTP
import WebRow.HTTP.Response (HTTPResponse(..), Parts) as HTTP.Response
import WebRow.HTTP.Response (HTTPResponse, SetHeader, SetHeaderF(..), _httpExcept, _setHeader)
import WebRow.HTTP.Response.Headers (setHeaderOnParts)
import WebRow.HTTP.Response.Types (Body(..))

data Response res
  = HTTPException HTTP.HTTPException HTTPure.Headers
  | HTTPResponse { parts ∷ HTTP.Response.Parts, ctx ∷ res }

derive instance functorResponse ∷ Functor Response

status ∷ ∀ res. Response res → Maybe Status
status (HTTPResponse { parts: { status: s } }) = Just s

status _ = Nothing

bodyString ∷ ∀ res. Response res → Maybe String
bodyString (HTTPResponse { parts: { body: BodyString b } }) = Just b

bodyString _ = Nothing

-- type Response' body res = Response body (Variant res)
type Render eff res
  = res → Run eff HTTPResponse

runRender ∷
  ∀ eff res.
  Render eff res →
  Run eff res →
  Run eff (Response res)
runRender r rCtx = do
  ctx ← rCtx
  HTTP.Response.HTTPResponse parts ← r ctx
  pure $ HTTPResponse { parts, ctx }

runHTTPExcept ∷
  ∀ eff res.
  Run (HTTPExcept + eff) (Response res) →
  Run eff (Response res)
runHTTPExcept = catchAt _httpExcept (\e → pure $ HTTPException e HTTPure.Headers.empty)

runSetHeader ∷
  ∀ res eff.
  Run (SetHeader + eff) (Response res) →
  Run eff (Response res)
runSetHeader =
  Run.run
    $ Run.on _setHeader go Run.send
  where
  go (SetHeaderF k v a) = pure (set k v <$> a)

  set k v (HTTPException e h) = HTTPException e (HTTPure.header k v <> h)

  set k v (HTTPResponse { ctx, parts }) = HTTPResponse { ctx, parts: (setHeaderOnParts k v parts) }
