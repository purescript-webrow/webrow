module WebRow.Response where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Variant (SProxy(..), Variant, inj, on)
import Data.Variant.Internal (FProxy)
import Effect.Aff.Class (class MonadAff)
import HTTPure (Headers, Response, badGateway', badRequest', forbidden', internalServerError', methodNotAllowed', notFound', notImplemented', serviceUnavailable', unauthorized') as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import Run (Run)
import Run as Run

type Body = String

data ClientError
  = BadRequest HTTPure.Headers Body
  | Unauthorized HTTPure.Headers
  | Forbidden HTTPure.Headers
  | NotFound HTTPure.Headers
  | MethodNotAllowed HTTPure.Headers

data ServerError
  = InternalServerError HTTPure.Headers Body
  | NotImplemented HTTPure.Headers
  | BadGateway HTTPure.Headers
  | ServiceUnavailable HTTPure.Headers

type Response res = Variant
  ( clientError ∷ ClientError
  , serverError ∷ ServerError
  | res
  )

newtype ResponseF res a = ResponseF (Response res)
derive instance functorResponseF ∷ Functor (ResponseF res)
derive instance newtypeResponseF ∷ Newtype (ResponseF res a) _

type RESPONSE res = FProxy (ResponseF res)

_response = SProxy ∷ SProxy "response"

-- | Fully interpret the Response effect returning the Response variant
runResponse
  ∷ ∀ res eff
  . Run ( response ∷ RESPONSE res | eff ) (Response res)
  → Run eff (Response res)
runResponse = runResponseWith (pure <<< pure)

-- | Fully interpret the Response effect with a handler
runResponseWith
  ∷ ∀ a res eff
  . ( Response res
    → Run eff (Run ( response ∷ RESPONSE res | eff ) a)
    )
  → Run ( response ∷ RESPONSE res | eff ) a
  → Run eff a
runResponseWith f = Run.run (Run.on _response (f <<< un ResponseF) Run.send)

response ∷
  ∀ res eff a
  . Response res
  → Run ( response ∷ RESPONSE res | eff ) a
response v = Run.lift _response (ResponseF v)

_clientError = SProxy ∷ SProxy "clientError"

clientError ∷ ∀ a eff res.  ClientError → Run ( response ∷ RESPONSE res | eff) a
clientError = response <<< inj _clientError

badRequest ∷ ∀ a eff res. HTTPure.Headers → Body → Run (response ∷ RESPONSE res | eff) a
badRequest headers = clientError <<< BadRequest headers

badRequest' ∷ ∀ a eff res. Body → Run (response ∷ RESPONSE res | eff) a
badRequest' = badRequest HTTPure.Headers.empty

badRequest'' ∷ ∀ a eff res. Run (response ∷ RESPONSE res | eff) a
badRequest'' = badRequest HTTPure.Headers.empty ""

unauthorized ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
unauthorized = clientError <<< Unauthorized

forbidden ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
forbidden = clientError <<< Forbidden

notFound ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
notFound headers = clientError $ NotFound headers

methodNotAllowed ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
methodNotAllowed headers = clientError $ MethodNotAllowed headers

methodNotAllowed' ∷ ∀ a eff res. Run ( response ∷ RESPONSE res | eff) a
methodNotAllowed' = methodNotAllowed Headers.empty

_serverError = SProxy ∷ SProxy "serverError"

serverError ∷ ∀ a eff res.  ServerError → Run ( response ∷ RESPONSE res | eff) a
serverError = response <<< inj _serverError

internalServerError ∷ ∀ a eff res. HTTPure.Headers → Body → Run ( response ∷ RESPONSE res | eff) a
internalServerError headers body = serverError $ InternalServerError headers body

notImplemented ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
notImplemented headers = serverError $ NotImplemented headers

badGateway ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
badGateway headers = serverError $ BadGateway headers

serviceUnavailable ∷ ∀ a eff res. HTTPure.Headers → Run ( response ∷ RESPONSE res | eff) a
serviceUnavailable headers = serverError $ ServiceUnavailable headers

-- | Transform encoded error responses into HttPure responses.
-- |
-- | We want to probably turn this into a runner which
-- | also does some logging based on the Run stack
onHttpError ∷ ∀ m res. MonadAff m ⇒ (Variant res → m HTTPure.Response) → Response res → m HTTPure.Response
onHttpError case'
  = case'
  # on _clientError handleClientError
  # on _serverError handleServerError
  where
    handleClientError (BadRequest headers body) = HTTPure.badRequest' headers body
    handleClientError (Unauthorized headers) = HTTPure.unauthorized' headers
    handleClientError (Forbidden headers) = HTTPure.forbidden' headers
    handleClientError (NotFound headers) = HTTPure.notFound' headers
    handleClientError (MethodNotAllowed headers) = HTTPure.methodNotAllowed' headers

    handleServerError (InternalServerError headers body) = HTTPure.internalServerError' headers body
    handleServerError (NotImplemented headers) = HTTPure.notImplemented' headers
    handleServerError (BadGateway headers) = HTTPure.badGateway' headers
    handleServerError (ServiceUnavailable headers) = HTTPure.serviceUnavailable' headers
