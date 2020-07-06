module WebRow.Testing.HTTP.Response where

import Prelude

import HTTPure.Body (class Body) as HTTPure
import Run (Run)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, SetHeader)
import WebRow.HTTP (HTTPException) as HTTP
import WebRow.HTTP.Response (HTTPResponse) as HTTP
import WebRow.HTTP.Response (Parts) as HTTP.Response
import WebRow.HTTP.Response (Parts, runWith') as Response

data Response body
  = HTTPException HTTP.HTTPException
  | HTTPResponse (HTTP.Response.Parts body)

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (HTTPExcept + SetHeader + eff) (HTTP.HTTPResponse String)
  → Run eff (Response.Parts String)
run =
  Response.runWith' identity

