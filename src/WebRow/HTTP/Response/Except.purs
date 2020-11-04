module WebRow.HTTP.Response.Except where

import Prelude

import Data.Variant (SProxy(..))
import HTTPure (Headers, header) as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import HTTPure.Status (badGateway, badRequest, forbidden, internalServerError, methodNotAllowed, notFound, notImplemented, serviceUnavailable, temporaryRedirect, unauthorized) as Status
import Run (Run)
import Run.Except (EXCEPT, throwAt)
import WebRow.HTTP.Response.Types (Body(..), Parts)
import WebRow.Routing.Types (Url(..))

-- | TODO: We want to probably carry json payload
-- | which can be passed to services like:
-- | * sentry
-- | * console
-- | * systemd?
newtype HTTPException
  = HTTPException Parts

_httpExcept = SProxy ∷ SProxy "httpExcept"

type HTTPEXCEPT
  = EXCEPT HTTPException

type HTTPExcept r
  = ( httpExcept ∷ HTTPEXCEPT | r )

httpExcept ∷ ∀ a eff. HTTPException → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
httpExcept = throwAt _httpExcept

redirect ∷ ∀ a eff. Url → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
redirect (Url url) =
  httpExcept
    $ HTTPException
        { body: BodyString "", headers: HTTPure.header "Location" url, status: Status.temporaryRedirect }

badRequest ∷ ∀ a eff. HTTPure.Headers → Body → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
badRequest headers body = httpExcept (HTTPException { body, headers, status: Status.badRequest })

badRequest' ∷ ∀ a eff. Body → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
badRequest' = badRequest HTTPure.Headers.empty

badRequest'' ∷ ∀ a eff. Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
badRequest'' = badRequest HTTPure.Headers.empty (BodyString "")

unauthorized ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
unauthorized headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.unauthorized })

forbidden ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
forbidden headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.forbidden })

notFound ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
notFound headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.notFound })

methodNotAllowed ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
methodNotAllowed headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.methodNotAllowed })

methodNotAllowed' ∷ ∀ a eff. Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
methodNotAllowed' = methodNotAllowed Headers.empty

internalServerError ∷ ∀ a eff. HTTPure.Headers → Body → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
internalServerError headers body = httpExcept (HTTPException { body, headers, status: Status.internalServerError })

internalServerError' ∷ ∀ a eff. Body → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
internalServerError' = internalServerError Headers.empty

notImplemented ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
notImplemented headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.notImplemented })

badGateway ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
badGateway headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.badGateway })

serviceUnavailable ∷ ∀ a eff. HTTPure.Headers → Run ( httpExcept ∷ EXCEPT HTTPException | eff ) a
serviceUnavailable headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.serviceUnavailable })
