module WebRow.HTTP.Response.Except where

import Prelude

import HTTPure (Headers, header) as HTTPure
import HTTPure.Headers (empty) as HTTPure.Headers
import HTTPure.Headers (empty) as Headers
import HTTPure.Status (badGateway, badRequest, forbidden, internalServerError, methodNotAllowed, notFound, notImplemented, serviceUnavailable, temporaryRedirect, unauthorized) as Status
import Run (Run)
import Run.Except (Except, EXCEPT, throwAt)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.HTTP.Response.Types (Body(..), Parts)
import WebRow.Routing.Types (Url(..))

-- | TODO: We want to probably carry json payload
-- | which can be passed to services like:
-- | * sentry
-- | * console
-- | * systemd?
newtype HTTPException
  = HTTPException Parts

_httpExcept = Proxy ∷ Proxy "httpExcept"

type HTTPExcept = Except HTTPException

type HTTPEXCEPT r = ( httpExcept ∷ HTTPExcept | r )

httpExcept ∷ ∀ a eff. HTTPException → Run ( HTTPEXCEPT + eff ) a
httpExcept = throwAt _httpExcept

redirect ∷ ∀ a eff. Url → Run ( HTTPEXCEPT + eff ) a
redirect (Url url) =
  httpExcept
    $ HTTPException
        { body: BodyString "", headers: HTTPure.header "Location" url, status: Status.temporaryRedirect }

badRequest ∷ ∀ a eff. HTTPure.Headers → Body → Run (HTTPEXCEPT + eff ) a
badRequest headers body = httpExcept (HTTPException { body, headers, status: Status.badRequest })

badRequest' ∷ ∀ a eff. Body → Run ( HTTPEXCEPT +  eff ) a
badRequest' = badRequest HTTPure.Headers.empty

badRequest'' ∷ ∀ a eff. Run ( HTTPEXCEPT +  eff ) a
badRequest'' = badRequest HTTPure.Headers.empty (BodyString "")

unauthorized ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
unauthorized headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.unauthorized })

forbidden ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
forbidden headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.forbidden })

notFound ∷ ∀ a eff. HTTPure.Headers → Body → Run ( HTTPEXCEPT +  eff ) a
notFound headers body = httpExcept (HTTPException { body, headers, status: Status.notFound })

methodNotAllowed ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
methodNotAllowed headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.methodNotAllowed })

methodNotAllowed' ∷ ∀ a eff. Run ( HTTPEXCEPT +  eff ) a
methodNotAllowed' = methodNotAllowed Headers.empty

internalServerError ∷ ∀ a eff. HTTPure.Headers → Body → Run ( HTTPEXCEPT +  eff ) a
internalServerError headers body = httpExcept (HTTPException { body, headers, status: Status.internalServerError })

internalServerError' ∷ ∀ a eff. Body → Run ( HTTPEXCEPT +  eff ) a
internalServerError' = internalServerError Headers.empty

notImplemented ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
notImplemented headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.notImplemented })

badGateway ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
badGateway headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.badGateway })

serviceUnavailable ∷ ∀ a eff. HTTPure.Headers → Run ( HTTPEXCEPT +  eff ) a
serviceUnavailable headers = httpExcept (HTTPException { body: BodyString "", headers, status: Status.serviceUnavailable })
