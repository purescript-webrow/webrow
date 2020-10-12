module WebRow.Testing.HTTP.Response where

import Prelude
import HTTPure (Headers, header) as HTTPure
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import Prim.Row (class Union) as Row
import Run (Run)
import Run (on, run, send) as Run
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept)
import WebRow.HTTP (HTTPException) as HTTP
import WebRow.HTTP.Response (HTTPResponse(..), Parts) as HTTP.Response
import WebRow.HTTP.Response (HTTPResponse, SetHeader, SetHeaderF(..), _httpExcept, _setHeader)
import WebRow.HTTP.Response.Headers (setHeaderOnParts)

data Response body res
  = HTTPException HTTP.HTTPException HTTPure.Headers
  | HTTPResponse { parts ∷ HTTP.Response.Parts body, ctx ∷ res }

derive instance functorResponse ∷ Functor (Response body)

-- type Response' body res = Response body (Variant res)
type Render eff body res
  = res → Run eff (HTTPResponse body)

runRender ∷
  ∀ body eff res.
  HTTPure.Body body ⇒
  Render eff body res →
  Run eff res →
  Run eff (Response body res)
runRender r rCtx = do
  ctx ← rCtx
  HTTP.Response.HTTPResponse parts ← r ctx
  pure $ HTTPResponse { parts, ctx }

runHTTPExcept ∷
  ∀ body eff res.
  HTTPure.Body body ⇒
  Run (HTTPExcept + eff) (Response body res) →
  Run eff (Response body res)
runHTTPExcept = catchAt _httpExcept (\e → pure $ HTTPException e HTTPure.Headers.empty)

runSetHeader ∷
  ∀ body res eff.
  Run (SetHeader + eff) (Response body res) →
  Run eff (Response body res)
runSetHeader =
  Run.run
    $ Run.on _setHeader go Run.send
  where
  go (SetHeaderF k v a) = pure (set k v <$> a)

  set k v (HTTPException e h) = HTTPException e (HTTPure.header k v <> h)

  set k v (HTTPResponse { ctx, parts }) = HTTPResponse { ctx, parts: (setHeaderOnParts k v parts) }
