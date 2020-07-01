module WebRow.HTTP.Response where

import Prelude

import Control.Bind (bindFlipped)
import Data.Newtype (class Newtype, un)
import Effect.Aff.Class (liftAff)
import HTTPure (Headers, Response, Status, response') as HTTPure
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Headers (empty) as Headers
import HTTPure.Status (ok) as Status
import Run (Run)
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow, EffRow)
import WebRow.HTTP.Response.Except (HTTPExcept, runHTTPExcept)
import WebRow.HTTP.Response.SetHeader (runSetHeader, SetHeader)

type Parts body =
  { body ∷ body, headers ∷ HTTPure.Headers, status ∷ HTTPure.Status }

-- | A tiny wrapper around response which allows us
-- | to inspect an response during testing.
newtype HTTPResponse body = HTTPResponse (Parts body)
derive instance newtypeHTTPResponse ∷ Newtype (HTTPResponse body) _

run
  ∷ ∀ body eff
  . HTTPure.Body body
  ⇒ Run (AffRow + EffRow + SetHeader + HTTPExcept + eff) (HTTPResponse body)
  → Run (AffRow + EffRow + eff) HTTPure.Response
run = runHTTPExcept <<< runHTTPResponse <<< runSetHeader <<< map (un HTTPResponse)
  where
    runHTTPResponse = bindFlipped (\r → HTTPure.response' r.status r.headers r.body)

runWith
  ∷ ∀ body body' eff
  . HTTPure.Body body'
  ⇒ (body → Run (AffRow + EffRow + HTTPExcept + eff) body')
  → Run (AffRow + EffRow + SetHeader + HTTPExcept + eff) (HTTPResponse body)
  → Run (AffRow + EffRow + eff) HTTPure.Response
runWith toBody = runHTTPExcept <<< runHTTPResponse <<< runSetHeader <<< map (un HTTPResponse)
  where
    runHTTPResponse = bindFlipped \r → do
      body ← toBody r.body
      liftAff $ HTTPure.response' r.status r.headers body

ok ∷ ∀ eff. String → Run eff (HTTPResponse String)
ok body = pure $ HTTPResponse { body, headers: Headers.empty, status: Status.ok }

