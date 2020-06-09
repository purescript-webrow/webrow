module WebRow.Response where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Variant (SProxy(..), Variant, case_, inj, on)
import Data.Variant.Internal (FProxy)
import HTTPure (Headers, Response, badGateway', badRequest', forbidden', header, internalServerError', methodNotAllowed', notFound', notImplemented', serviceUnavailable', temporaryRedirect', unauthorized') as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import Prim.Row as Row
import Run (AFF, EFFECT, Run)
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

-- | Handling typed `route` and `printRoute`
-- | inside redirect was to heavy for inference.
-- | Let's leave it as a plain String
-- | on this level ;-)
-- |
-- | Handle all redirects types by
-- | providing type...
-- | https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections

type Response res = Variant
  ( clientError ∷ ClientError
  , serverError ∷ ServerError
  , redirect ∷ String
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
  . Row.Union eff () eff
  ⇒ Run ( response ∷ RESPONSE res | eff ) (Response res)
  → Run eff (Response res)
runResponse = runResponseWith (pure <<< pure)

mapResponse
  ∷ ∀ a res res' eff
  . Row.Union eff (response ∷ RESPONSE res') ( response ∷ RESPONSE res' | eff )
  ⇒ (Response res → Response res')
  → Run ( response ∷ RESPONSE res  | eff ) a
  → Run ( response ∷ RESPONSE res' | eff ) a
mapResponse f = interpretResponseWith \res → response $ f res

-- | Fully interpret the Response effect with a handler
interpretResponseWith
  ∷ ∀ a res eff eff' _eff
  . Row.Union eff _eff eff'
  ⇒ (Response res → Run eff' a)
  → Run ( response ∷ RESPONSE res | eff ) a
  → Run eff' a
interpretResponseWith f = runResponseWith (map pure <<< f)

-- | Less restrictive version of `interpretResponseWith`
runResponseWith
  ∷ ∀ a res eff eff' _eff
  . Row.Union eff _eff eff'
  ⇒ (Response res → Run eff' (Run ( response ∷ RESPONSE res | eff ) a))
  → Run ( response ∷ RESPONSE res | eff ) a
  → Run eff' a
runResponseWith f = Run.run (Run.on _response (f <<< un ResponseF) (Run.send >>> Run.expand))

response ∷
  ∀ a eff res
  . Response res
  → Run ( response ∷ RESPONSE res | eff ) a
response v = Run.lift _response (ResponseF v)

_redirect = SProxy ∷ SProxy "redirect"

redirect ∷ ∀ a eff res. String → Run (response ∷ RESPONSE res | eff) a
redirect = response <<< inj _redirect

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
onHttpError
  ∷ ∀ eff res
  . (Variant res → Run (aff ∷ AFF, effect ∷ EFFECT | eff) HTTPure.Response)
  → Response res
  → Run (aff ∷ AFF, effect ∷ EFFECT | eff) HTTPure.Response
onHttpError case'
  = case'
  # on _clientError handleClientError
  # on _serverError handleServerError
  # on _redirect handleRedirect
  where
    handleRedirect url = do
      HTTPure.temporaryRedirect' (HTTPure.header "Location" url) ""

    handleClientError (BadRequest headers body) = HTTPure.badRequest' headers body
    handleClientError (Unauthorized headers) = HTTPure.unauthorized' headers
    handleClientError (Forbidden headers) = HTTPure.forbidden' headers
    handleClientError (NotFound headers) = HTTPure.notFound' headers
    handleClientError (MethodNotAllowed headers) = HTTPure.methodNotAllowed' headers

    handleServerError (InternalServerError headers body) = HTTPure.internalServerError' headers body
    handleServerError (NotImplemented headers) = HTTPure.notImplemented' headers
    handleServerError (BadGateway headers) = HTTPure.badGateway' headers
    handleServerError (ServiceUnavailable headers) = HTTPure.serviceUnavailable' headers

onHttpErrorBase
  ∷ ∀ eff
  . Response ()
  → Run (aff ∷ AFF, effect ∷ EFFECT | eff) HTTPure.Response
onHttpErrorBase = onHttpError case_

runResponseBase
  ∷ ∀ eff
  . Row.Union eff () eff
  ⇒ Run ( response ∷ RESPONSE (), aff ∷ AFF, effect ∷ EFFECT | eff) HTTPure.Response
  → Run (                         aff ∷ AFF, effect ∷ EFFECT | eff) HTTPure.Response
runResponseBase = runResponseWith (onHttpErrorBase >>> pure)
