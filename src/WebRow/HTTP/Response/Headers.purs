module WebRow.HTTP.Response.Headers where

import Prelude

import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import HTTPure (header) as HTTPure
import Run (Run)
import Run (lift, on, run, send) as Run
import Type.Row (type (+))
import WebRow.HTTP.Response.Except (HTTPException(..))
import WebRow.HTTP.Response.Types (HTTPResponse(..), Parts)

-- | TODO: Change to `SetHeaders (Tuple String String)`
data SetHeaderF a
  = SetHeaderF String String a

derive instance functorModifyF ∷ Functor SetHeaderF

type SETHEADER
  = FProxy SetHeaderF

type SetHeader r
  = ( setHeader ∷ SETHEADER | r )

_setHeader = SProxy ∷ SProxy "setHeader"

setHeader ∷
  ∀ eff.
  String →
  String →
  Run ( setHeader ∷ SETHEADER | eff ) Unit
setHeader k v = Run.lift _setHeader (SetHeaderF k v unit)

setContentType ∷ ∀ eff. MediaType → Run (SetHeader + eff) Unit
setContentType (MediaType t) =
  setHeader "Content-Type" t

setHeaderOnParts ∷ String → String → Parts → Parts
setHeaderOnParts k v parts = parts { headers = HTTPure.header k v <> parts.headers }

setHeaderOnHTTPException ∷ String → String → HTTPException → HTTPException
setHeaderOnHTTPException k v (HTTPException parts) = HTTPException $ setHeaderOnParts k v parts

setHeaderOnHTTPResponse ∷ String → String → HTTPResponse → HTTPResponse
setHeaderOnHTTPResponse k v (HTTPResponse parts) = HTTPResponse $ setHeaderOnParts k v parts

runSetHeader ∷
  ∀ eff.
  Run (SetHeader + eff) HTTPResponse →
  Run (eff) HTTPResponse
runSetHeader =
  Run.run
    $ Run.on _setHeader setOnResponse Run.send
  where
  setOnResponse (SetHeaderF k v a) = pure $ setHeaderOnHTTPResponse k v <$> a
