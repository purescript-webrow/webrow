module WebRow.HTTP.Response where

import Prelude

import Data.Newtype (class Newtype, un)
import HTTPure (Headers, Response, Status) as HTTPure
import HTTPure (empty) as HTTPure.Headers
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Body (write) as Body
import HTTPure.Status (ok) as HTTPure.Status
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

runWith
  ∷ ∀ body eff r
  . HTTPure.Body body
  ⇒ (∀ b. HTTPure.Body b ⇒ Parts b → r)
  → Run (SetHeader + HTTPExcept + eff) (HTTPResponse body)
  → Run eff r
runWith toResponse action = action'
  where
    action' = runHTTPExcept <<< map toResponse <<< runSetHeader <<< map (un HTTPResponse) $ action
    runHTTPExcept = catchAt _httpExcept \(HTTPException b) → pure (toResponse b)

-- | Simplified monomorphic version for testing.
runWith'
  ∷ ∀ eff r
  . (Parts String → r)
  → Run (SetHeader + HTTPExcept + eff) (HTTPResponse String)
  → Run eff r
runWith' toResponse action = action'
  where
    action' = runHTTPExcept <<< map toResponse <<< runSetHeader <<< map (un HTTPResponse) $ action
    runHTTPExcept = catchAt _httpExcept \(HTTPException b) → pure (toResponse b)

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (SetHeader + HTTPExcept + eff) (HTTPResponse body)
  → Run eff HTTPure.Response
run = runWith toResponse
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

ok ∷ ∀ eff. String → Run eff (HTTPResponse String)
ok body = pure $ HTTPResponse { body, headers: HTTPure.Headers.empty, status: HTTPure.Status.ok }

