module WebRow.Crypto
  ( _crypto
  , secret
  , signJson
  , sign
  , unsignJson
  , unsign
  , Crypto
  )
  where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import HTTPure (empty) as Headers
import Node.Simple.Jwt (Secret)
import Run (Run, SProxy(..))
import Run.Reader (READER, askAt)
import Type.Row (type (+))
import WebRow.Crypto.Jwt (UnsignError)
import WebRow.Crypto.Jwt (sign, unsign) as Jwt
import WebRow.Crypto.String (sign, unsign) as String
import WebRow.HTTP.Response.Except (HTTPExcept, internalServerError)

_crypto = SProxy ∷ SProxy "crypto"

type Crypto r = (crypto ∷ READER Secret | r)

secret ∷ ∀ eff. Run (Crypto + eff) Secret
secret = askAt _crypto

-- | Use Types.Signed wrapper
signJson
  ∷ ∀ eff
  . Json
  → Run (Crypto + HTTPExcept + eff) String
signJson json = do
  sec ← askAt _crypto
  case Jwt.sign sec json of
    Left e → internalServerError Headers.empty "Serious problem..."
    Right s → pure s

sign
  ∷ ∀ eff
  . String
  → Run (Crypto + HTTPExcept + eff) String
sign str = do
  sec ← askAt _crypto
  case String.sign sec str of
    Left e → internalServerError Headers.empty "Serious problem..."
    Right s → pure s

unsignJson
  ∷ ∀ eff
  . String
  → Run (Crypto + eff) (Either UnsignError Json)
unsignJson json =
  askAt _crypto <#> \s → Jwt.unsign s json

unsign
  ∷ ∀ eff
  . String
  → Run (Crypto + eff) (Either UnsignError String)
unsign json =
  askAt _crypto <#> \s → String.unsign s json
