module WebRow.Crypto
  ( _crypto
  , run
  , secret
  , signJson
  , sign
  , unsignJson
  , unsign
  , CRYPTO
  , Crypto
  , module Types
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import HTTPure (empty) as Headers
import Run (Run)
import Run.Reader (Reader, askAt, runReaderAt)
import Type.Row (type (+))
import Type.Prelude (Proxy(..))
import WebRow.Crypto.Jwt.Node (UnsignError)
import WebRow.Crypto.Jwt.Node (sign, unsign) as Jwt
import WebRow.Crypto.Jwt.Node.String (sign, unsign) as String
import WebRow.Crypto.Types (Secret(..)) as Types
import WebRow.Crypto.Types (Secret)
import WebRow.HTTP.Response.Except (HTTPEXCEPT, internalServerError)
import WebRow.HTTP.Response.Types (Body(..))

_crypto = Proxy ∷ Proxy "crypto"

type Crypto = Reader Secret

type CRYPTO r = ( crypto ∷ Crypto | r )

secret ∷ ∀ eff. Run (CRYPTO + eff) Secret
secret = askAt _crypto

-- | TODO: Should we handle errors through custom exception?
signJson ∷
  ∀ eff.
  Json →
  Run (CRYPTO + HTTPEXCEPT + eff) String
signJson json = do
  sec ← askAt _crypto
  case Jwt.sign sec json of
    Left _ → internalServerError Headers.empty $ BodyString "Serious problem..."
    Right s → pure s

sign ∷
  ∀ eff.
  String →
  Run (CRYPTO + HTTPEXCEPT + eff) String
sign str = do
  sec ← askAt _crypto
  case String.sign sec str of
    Left _ → internalServerError Headers.empty $ BodyString "Serious problem..."
    Right s → pure s

unsignJson ∷
  ∀ eff.
  String →
  Run (CRYPTO + eff) (Either UnsignError Json)
unsignJson json = askAt _crypto <#> \s → Jwt.unsign s json

unsign ∷
  ∀ eff.
  String →
  Run (CRYPTO + eff) (Either UnsignError String)
unsign json = do
  askAt _crypto <#> \s → String.unsign s json

run ∷ ∀ eff. Secret → Run (CRYPTO + eff) ~> Run eff
run s = runReaderAt _crypto s
