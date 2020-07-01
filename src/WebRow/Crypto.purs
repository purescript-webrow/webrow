module WebRow.Crypto where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut (Json)
import Data.Argonaut (fromString, jsonParser, stringify, toString) as Argonaut
import Data.Either (Either(..), note)
import Node.Simple.Jwt (Algorithm(..), decode, encode, fromString, toString) as Jwt
import Node.Simple.Jwt (JwtError)
import Run (EFFECT, Run, SProxy(..), liftEffect)
import Run.Reader (READER, askAt)
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)

newtype Secret = Secret String

_crypto = SProxy ∷ SProxy "crypto"

type Crypto r = (crypto ∷ READER Secret | r)

newtype Signed = Signed String
newtype Unsigned = Unsigned String

signJson
  ∷ ∀ eff
  . Json
  → Run ( Crypto + EffRow + eff ) String
signJson json = do
  Secret secret ← askAt _crypto
  liftEffect $ Jwt.toString <$> (Jwt.encode secret Jwt.HS512 (Argonaut.stringify json))

sign
  ∷ ∀ eff
  . String
  → Run
    ( crypto ∷ READER Secret
    , effect ∷ EFFECT
    | eff
    )
    String
sign = signJson <<< Argonaut.fromString


data UnsignError
  = JwtError JwtError
  | JsonParserError String

unsignJson
  ∷ ∀ eff
  . String
  → Run
    ( crypto ∷ READER Secret
    , effect ∷ EFFECT
    | eff
    )
    (Either UnsignError Json)
unsignJson str = do
  Secret secret ← askAt _crypto
  liftEffect (Jwt.decode secret (Jwt.fromString str)) >>= case _ of
    Left err → pure $ Left $ JwtError err
    Right payload → case Argonaut.jsonParser payload of
      Left e → pure $ Left (JsonParserError e)
      Right json → pure $ Right json
      -- | TODO: add support for expiration date
      -- exp  toObject >=> Object.lookup "exp"

unsign
  ∷ ∀ eff
  . String
  → Run
    ( crypto ∷ READER Secret
    , effect ∷ EFFECT
    | eff
    )
    (Either UnsignError String)
unsign = unsignJson >=> bindFlipped toString' >>> pure
  where
    err = JsonParserError "String extraction error"
    toString' = note err <$> Argonaut.toString
