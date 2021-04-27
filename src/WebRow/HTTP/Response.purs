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
import HTTPure.Body (write) as Body
import HTTPure.Status (found, ok) as HTTPure.Status
import Run (Run)
import Run.Except (catchAt)
import Type.Row (type (+))
import WebRow.HTTP.Response.BodyWriter (BodyWriter(..)) as BodyWriter
import WebRow.HTTP.Response.Except (_httpExcept, HTTPException(..), HTTPExcept)
import WebRow.HTTP.Response.Except (_httpExcept, HTTPException(..), HTTPExcept, notFound) as Except
import WebRow.HTTP.Response.Headers (runSetHeader, SetHeader)
import WebRow.HTTP.Response.Headers (runSetHeader, setHeader, _setHeader, SetHeader, SetHeaderF(..)) as Headers
import WebRow.HTTP.Response.Types (Body(..), HTTPResponse(..), Parts)
import WebRow.HTTP.Response.Types (ContentDisposition(..), HTTPResponse(..), Parts) as Types
import WebRow.Routing.Types (Url(..))

run ∷
  ∀ eff.
  Run (SetHeader + HTTPExcept + eff) HTTPResponse →
  Run eff HTTPure.Response
run action = action'
  where
  action' = runHTTPExcept <<< map fromResponse <<< runSetHeader $ action

  runHTTPExcept = catchAt _httpExcept (fromException >>> pure)

  fromResponse (HTTPResponse parts) = fromParts parts

  fromException (HTTPException parts) = fromParts parts

  fromParts ∷ Parts → HTTPure.Response
  fromParts { body, headers, status } =
    { status
    , headers
    , writeBody:
      case body of
        BodyString string → Body.write string
        BodyBuffer buffer → Body.write buffer
        BodyStream stream → Body.write stream
        BodyWriter writer → Body.write $ BodyWriter.BodyWriter writer
    }

ok ∷ ∀ eff. String → Run eff HTTPResponse
ok body =
  pure
    $ HTTPResponse { body: BodyString body, headers: HTTPure.Headers.empty, status: HTTPure.Status.ok }

okWithHeaders ∷ ∀ eff. Headers → String → Run eff HTTPResponse
okWithHeaders headers body =
  pure
    $ HTTPResponse { body: BodyString body, headers, status: HTTPure.Status.ok }

found ∷ ∀ eff. Url → Run eff HTTPResponse
found (Url location) =
  pure
    $ HTTPResponse { body: BodyString "", headers: HTTPure.header "location" location, status: HTTPure.Status.found }
