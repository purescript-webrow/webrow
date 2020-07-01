module WebRow.Testing.HTTP.Response where

import Prelude

import Data.Newtype (un)
import HTTPure.Body (class Body) as HTTPure
import Run (Run)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, SetHeader)
import WebRow.HTTP (HTTPException) as HTTP
import WebRow.HTTP.Response (HTTPResponse(..)) as HTTP
import WebRow.HTTP.Response (Parts) as HTTP.Response
import WebRow.HTTP.Response.Except (runHTTPExceptWith)
import WebRow.HTTP.Response.SetHeader (runSetHeader)

data Response body
  = HTTPException HTTP.HTTPException
  | HTTPResponse (HTTP.Response.Parts body)

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (HTTPExcept + SetHeader + eff) (HTTP.HTTPResponse body)
  → Run eff (Response body)
run =
  runHTTPExceptWith (HTTPException >>> pure) <<< map HTTPResponse <<< runSetHeader <<< map (un HTTP.HTTPResponse)

