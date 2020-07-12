module WebRow.Testing.HTTP.Response where

import Prelude

import Data.Newtype (unwrap)
import HTTPure (Headers, header) as HTTPure
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import Run (Run)
import Run (on, run, send) as Run
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept)
import WebRow.HTTP (HTTPException, HTTPResponse) as HTTP
import WebRow.HTTP.Response (Parts) as HTTP.Response
import WebRow.HTTP.Response (SetHeader, SetHeaderF(..), _httpExcept, _setHeader)
import WebRow.HTTP.Response.Headers (setHeaderOnParts)

-- | TODO: We should extend response by template
-- | "rendering context" so we can inspect these pieces
-- | during testing: `Response body context`
data Response body
  = HTTPException HTTP.HTTPException HTTPure.Headers
  | HTTPResponse (HTTP.Response.Parts body)

runSetHeader
  ∷ ∀ body eff
  . Run (SetHeader + eff) (Response body)
  → Run eff (Response body)
runSetHeader = Run.run $
  Run.on _setHeader go Run.send
  where
    go (SetHeaderF k v a) = pure (set k v <$> a)

    set k v (HTTPException e h) = HTTPException e (HTTPure.header k v <> h)
    set k v (HTTPResponse parts) = HTTPResponse (setHeaderOnParts k v parts)

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (HTTPExcept + SetHeader + eff) (HTTP.HTTPResponse body)
  → Run eff (Response body)
run
  = runSetHeader
  <<< catchAt _httpExcept (\e → pure $ HTTPException e HTTPure.Headers.empty)
  <<< map (HTTPResponse <<< unwrap)


