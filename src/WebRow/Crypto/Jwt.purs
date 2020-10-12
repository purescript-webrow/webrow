module WebRow.Crypto.Jwt where

import Prelude
import Data.Argonaut (jsonParser) as Argonaut
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String (split) as String
import Data.String.Base64 (decode) as Base64
import Data.String.Pattern (Pattern(..))
import WebRow.Crypto.Types (Unverified(..))

unverified ∷
  String →
  Maybe Unverified
unverified jwt = case String.split (Pattern ".") jwt of
  [ _, payloadSegment, _ ] → do
    payload ← hush $ Base64.decode payloadSegment
    json ← hush $ Argonaut.jsonParser payload
    pure $ Unverified json
  otherwise → Nothing
