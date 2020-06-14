module WebRow.HTTPError where

import Prelude

import Data.Variant (SProxy(..))
import HTTPure (Headers, Response, badGateway', badRequest', forbidden', internalServerError', methodNotAllowed', notFound', notImplemented', serviceUnavailable', unauthorized') as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT, catchAt, throwAt)

type Body = String

data HTTPError
  = BadGateway HTTPure.Headers
  | BadRequest HTTPure.Headers Body
  | Forbidden HTTPure.Headers
  | InternalServerError HTTPure.Headers Body
  | MethodNotAllowed HTTPure.Headers
  | NotFound HTTPure.Headers
  | NotImplemented HTTPure.Headers
  | ServiceUnavailable HTTPure.Headers
  | Unauthorized HTTPure.Headers

_httpError = SProxy ∷ SProxy "httpError"

type HTTPERROR = EXCEPT HTTPError

type HttpError r = (httpError ∷ HTTPERROR | r)

httpError ∷ ∀ a eff. HTTPError → Run (httpError ∷ EXCEPT HTTPError | eff) a
httpError = throwAt _httpError

badRequest ∷ ∀ a eff. HTTPure.Headers → Body → Run (httpError ∷ EXCEPT HTTPError | eff) a
badRequest headers = httpError <<< BadRequest headers

badRequest' ∷ ∀ a eff. Body → Run (httpError ∷ EXCEPT HTTPError | eff) a
badRequest' = badRequest HTTPure.Headers.empty

badRequest'' ∷ ∀ a eff. Run (httpError ∷ EXCEPT HTTPError | eff) a
badRequest'' = badRequest HTTPure.Headers.empty ""

unauthorized ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
unauthorized = httpError <<< Unauthorized

forbidden ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
forbidden = httpError <<< Forbidden

notFound ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
notFound = httpError <<< NotFound

methodNotAllowed ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
methodNotAllowed headers = httpError $ MethodNotAllowed headers

methodNotAllowed' ∷ ∀ a eff. Run (httpError ∷ EXCEPT HTTPError | eff) a
methodNotAllowed' = methodNotAllowed Headers.empty

internalServerError ∷ ∀ a eff. HTTPure.Headers → Body → Run (httpError ∷ EXCEPT HTTPError | eff) a
internalServerError headers = httpError <<< InternalServerError headers

notImplemented ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
notImplemented = httpError <<< NotImplemented

badGateway ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
badGateway headers = httpError $ BadGateway headers

serviceUnavailable ∷ ∀ a eff. HTTPure.Headers → Run (httpError ∷ EXCEPT HTTPError | eff) a
serviceUnavailable headers = httpError $ ServiceUnavailable headers

onHTTPError :: forall eff.
   Run
     ( aff :: AFF
     , effect :: EFFECT
     , httpError :: EXCEPT HTTPError
     | eff
     )
    HTTPure.Response
   -> Run
        ( aff :: AFF
        , effect :: EFFECT
        | eff
        )
        HTTPure.Response
onHTTPError
  = catchAt _httpError handle
  where
    handle (BadRequest headers body) = HTTPure.badRequest' headers body
    handle (Unauthorized headers) = HTTPure.unauthorized' headers
    handle (Forbidden headers) = HTTPure.forbidden' headers
    handle (NotFound headers) = HTTPure.notFound' headers
    handle (MethodNotAllowed headers) = HTTPure.methodNotAllowed' headers
    handle (InternalServerError headers body) = HTTPure.internalServerError' headers body
    handle (NotImplemented headers) = HTTPure.notImplemented' headers
    handle (BadGateway headers) = HTTPure.badGateway' headers
    handle (ServiceUnavailable headers) = HTTPure.serviceUnavailable' headers

