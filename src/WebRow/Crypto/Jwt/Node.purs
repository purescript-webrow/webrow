module WebRow.Crypto.Jwt.Node where

import Prelude
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Effect.Exception (Error, catchException) as Effect
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Node.Simple.Jwt (Algorithm(..), decode, encode, fromString, toString) as Jwt
import Node.Simple.Jwt (JwtError)
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Crypto.Types (Secret(..))

data UnsignError
  = JwtError JwtError
  | PossibleDecodingError Effect.Error

data SignError
  = PossibleEncodingError Effect.Error

sign ∷
  Secret →
  Json →
  Either SignError String
sign (Secret secret) json = unsafePerformEffect $ Effect.catchException (pure <<< Left <<< PossibleEncodingError) s
  where
  jsonToForeign ∷ Json → Foreign
  jsonToForeign = unsafeCoerce

  s = (Right <<< Jwt.toString) <$> (Jwt.encode secret Jwt.HS512 (jsonToForeign json))

unsign ∷
  Secret →
  String →
  Either UnsignError Json
unsign (Secret secret) str = unsafePerformEffect $ Effect.catchException (pure <<< Left <<< PossibleDecodingError) u
  where
  fromForeingJson ∷ Foreign → Json
  fromForeingJson = unsafeCoerce

  u =
    Jwt.decode secret (Jwt.fromString str)
      >>= case _ of
          Left err → pure $ Left $ JwtError err
          Right payload → pure $ Right (fromForeingJson payload)
