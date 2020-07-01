module WebRow.HTTP.Response.Except where

import Prelude

import Data.Variant (SProxy(..))
import HTTPure (Headers, Response, Status, header, response') as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import HTTPure.Status (badGateway, badRequest, forbidden, internalServerError, methodNotAllowed, notFound, notImplemented, serviceUnavailable, temporaryRedirect, unauthorized) as Status
import Run (Run)
import Run.Except (EXCEPT, catchAt, throwAt)
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow, EffRow)
import WebRow.HTTP.Types (Body)
import WebRow.Route (Url(..))

newtype HTTPException = HTTPException { body ∷ String, headers ∷ HTTPure.Headers, status ∷ HTTPure.Status }

_httpExcept = SProxy ∷ SProxy "httpExcept"

type HTTPEXCEPT = EXCEPT HTTPException

type HTTPExcept r = (httpExcept ∷ HTTPEXCEPT | r)

httpExcept ∷ ∀ a eff. HTTPException → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
httpExcept = throwAt _httpExcept

redirect ∷ ∀ a eff. Url → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
redirect (Url url) = httpExcept $ HTTPException
  { body: "", headers: HTTPure.header "Location" url, status: Status.temporaryRedirect }

badRequest ∷ ∀ a eff. HTTPure.Headers → Body → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
badRequest headers body = httpExcept (HTTPException { body, headers, status: Status.badRequest })

badRequest' ∷ ∀ a eff. Body → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
badRequest' = badRequest HTTPure.Headers.empty

badRequest'' ∷ ∀ a eff. Run (httpExcept ∷ EXCEPT HTTPException | eff) a
badRequest'' = badRequest HTTPure.Headers.empty ""

unauthorized ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
unauthorized headers = httpExcept (HTTPException { body: "", headers, status: Status.unauthorized })

forbidden ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
forbidden headers = httpExcept (HTTPException { body: "", headers, status: Status.forbidden })

notFound ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
notFound headers = httpExcept (HTTPException { body: "", headers, status: Status.notFound })

methodNotAllowed ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
methodNotAllowed headers = httpExcept (HTTPException { body: "", headers, status: Status.methodNotAllowed })

methodNotAllowed' ∷ ∀ a eff. Run (httpExcept ∷ EXCEPT HTTPException | eff) a
methodNotAllowed' = methodNotAllowed Headers.empty

internalServerError ∷ ∀ a eff. HTTPure.Headers → Body → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
internalServerError headers body = httpExcept (HTTPException { body, headers, status: Status.internalServerError })

notImplemented ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
notImplemented headers = httpExcept (HTTPException { body: "", headers, status: Status.notImplemented })

badGateway ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
badGateway headers = httpExcept (HTTPException { body: "", headers, status: Status.badGateway })

serviceUnavailable ∷ ∀ a eff. HTTPure.Headers → Run (httpExcept ∷ EXCEPT HTTPException | eff) a
serviceUnavailable headers = httpExcept (HTTPException { body: "", headers, status: Status.serviceUnavailable })

runHTTPExceptWith
  ∷ ∀ a eff
  . (HTTPException → Run eff a)
  → Run (HTTPExcept + eff) a
  → Run eff a
runHTTPExceptWith toResponse = catchAt _httpExcept \e → toResponse e

runHTTPExcept
  ∷ ∀ eff
  . Run (AffRow + EffRow + HTTPExcept + eff) HTTPure.Response
  → Run (AffRow + EffRow + eff) HTTPure.Response
runHTTPExcept
  = runHTTPExceptWith \(HTTPException { body, headers, status }) → HTTPure.response' status headers body

