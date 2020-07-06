module WebRow.Crypto.Jwt where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (jsonParser, stringify) as Argonaut
import Data.Either (Either(..))
import Effect.Exception (Error, catchException) as Effect
import Effect.Unsafe (unsafePerformEffect)
import Node.Simple.Jwt (Algorithm(..), decode, encode, fromString, toString) as Jwt
import Node.Simple.Jwt (JwtError, Secret)

data UnsignError
  = JwtError JwtError
  | JsonParserError String
  | PossibleDecodingError Effect.Error

data SignError = PossibleEncodingError Effect.Error

sign
  ∷ Secret
  → Json
  → Either SignError String
sign secret json =
  unsafePerformEffect $ Effect.catchException (pure <<< Left <<< PossibleEncodingError) s
  where
    s = (Right <<< Jwt.toString) <$> (Jwt.encode secret Jwt.HS512 (Argonaut.stringify json))

unsign
  ∷ Secret
  → String
  → Either UnsignError Json
unsign secret str =
  unsafePerformEffect $ Effect.catchException (pure <<< Left <<< PossibleDecodingError) u
  where
    u = Jwt.decode secret (Jwt.fromString str) >>= case _ of
      Left err → pure $ Left $ JwtError err
      Right payload → case Argonaut.jsonParser payload of
        Left e → pure $ Left (JsonParserError e)
        Right json → pure $ Right json

