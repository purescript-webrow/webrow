module WebRow.HTTP.Response
  ( module Except
  , module Headers
  , module Types
  , found
  , ok
  , okWithHeaders
  , run
  ) where

import Prelude

import HTTPure (Headers)
import HTTPure (Response, header) as HTTPure
import HTTPure (empty) as HTTPure.Headers
import HTTPure.Body (class Body) as HTTPure
import HTTPure.Body (write) as Body
import HTTPure.Status (found, ok) as HTTPure.Status
import Run (Run)
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP.Response.Except (_httpExcept, HTTPException(..), HTTPExcept)
import WebRow.HTTP.Response.Except (_httpExcept, HTTPException(..), HTTPExcept, notFound) as Except
import WebRow.HTTP.Response.Headers (runSetHeader, SetHeader)
import WebRow.HTTP.Response.Headers (runSetHeader, setHeader, _setHeader, SetHeader, SetHeaderF(..)) as Headers
import WebRow.HTTP.Response.Types (HTTPResponse(..), Parts)
import WebRow.HTTP.Response.Types (HTTPResponse(..), Parts) as Types
import WebRow.Routing.Types (Url(..))

run ∷
  ∀ body eff.
  HTTPure.Body body ⇒
  Run (SetHeader + HTTPExcept + eff) (HTTPResponse body) →
  Run eff HTTPure.Response
run action = action'
  where
  action' = runHTTPExcept <<< map fromResponse <<< runSetHeader $ action

  runHTTPExcept = catchAt _httpExcept (fromException >>> pure)

  fromResponse (HTTPResponse parts) = fromParts parts

  fromException (HTTPException parts) = fromParts parts

  fromParts ∷
    ∀ b.
    HTTPure.Body b ⇒
    Parts b → HTTPure.Response
  fromParts { body, headers, status } =
    { status
    , headers
    , writeBody: Body.write body
    }

ok ∷ ∀ eff. String → Run eff (HTTPResponse String)
ok body =
  pure
    $ HTTPResponse { body, headers: HTTPure.Headers.empty, status: HTTPure.Status.ok }

okWithHeaders ∷ ∀ eff. Headers → String → Run eff (HTTPResponse String)
okWithHeaders headers body =
  pure
    $ HTTPResponse { body, headers, status: HTTPure.Status.ok }

found ∷ ∀ eff. Url → Run eff (HTTPResponse String)
found (Url location) =
  pure
    $ HTTPResponse { body: "", headers: HTTPure.header "location" location, status: HTTPure.Status.found }
