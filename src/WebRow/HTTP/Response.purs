module WebRow.HTTP.Response where

import Prelude

import Data.Newtype (class Newtype, un)
import HTTPure (Headers, Response, Status) as HTTPure
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Body (write) as Body
import Run (Run)
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP.Response.Except (_httpExcept, HTTPException(..), HTTPExcept)
import WebRow.HTTP.Response.SetHeader (runSetHeader, SetHeader)

type Parts body =
  { body ∷ body, headers ∷ HTTPure.Headers, status ∷ HTTPure.Status }

-- | A tiny wrapper around response which enables
-- | inspection during testing.
newtype HTTPResponse body = HTTPResponse (Parts body)
derive instance newtypeHTTPResponse ∷ Newtype (HTTPResponse body) _

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (SetHeader + HTTPExcept + eff) (HTTPResponse body)
  → Run eff HTTPure.Response
run action = action'
  where
    toResponse
      ∷ ∀ b
      . HTTPure.Body b
      ⇒ Parts b → HTTPure.Response
    toResponse { body, headers, status } =
      { status
      , headers
      , writeBody: Body.write body
      }
    action' = runSetHeader <<< runHTTPExcept <<< map toResponse <<< map (un HTTPResponse) $ action
    runHTTPExcept = catchAt _httpExcept \(HTTPException b) → pure (toResponse b)

