module WebRow.Crypto where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..), lastIndexOf, replaceAll, splitAt, stripPrefix, stripSuffix)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, fromString, toString)
import Node.Crypto (randomBytes)
import Node.Crypto.Hash (Algorithm(SHA512), createHash)
import Node.Crypto.Hash as Hash
import Node.Crypto.Hmac (createHmac, digest, update)
import Node.Encoding (Encoding(..))
import Run (AFF, Run, liftAff)
import Run.Reader (READER, ask)

newtype Secret = Secret String
derive instance newtypeSecret ∷ Newtype Secret _

sign
  ∷ ∀ ctx eff
  . String
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      String
sign plain = do
  ctx ← ask
  liftAff $ liftEffect $ sign' ctx.secret plain

unsign
  ∷ ∀ ctx eff
  . String
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      (Either String String)
unsign signed = do
  secret ← ask <#> _.secret
  liftAff $ liftEffect $ runExceptT $ unsign' secret signed

urisafeBase64
  ∷ { encode ∷ Buffer → Effect String
    , decode ∷ String → Effect Buffer
    }
urisafeBase64 = { encode , decode }
  where
  stripEqualSuffix s =
    case stripSuffix (Pattern "==") s of
      Just s' → s'
      Nothing → s
  encode s =
    ( stripEqualSuffix
    <<< replaceAll (Pattern "/") (Replacement "_")
    <<< replaceAll (Pattern "+") (Replacement "-")
    )
    <$> toString Base64 s
  decode =
    flip fromString Base64
    <<< (_ <> "==")
    <<< replaceAll (Pattern "_") (Replacement "/")
    <<< replaceAll (Pattern "-") (Replacement "+")

hmac
  ∷ Secret
  → String
  → Effect String
hmac secret plain = do
  buf <- fromString plain UTF8
  urisafeBase64.encode
    =<< digest
    =<< flip update buf
    =<< createHmac SHA512 (unwrap secret)

hash ∷ String → Effect String
hash plain = do
  buf <- fromString plain UTF8
  urisafeBase64.encode
    =<< Hash.digest
    =<< flip Hash.update buf
    =<< createHash SHA512

randomSalt ∷ Effect String
randomSalt = randomBytes 8 >>= toString Hex

separator ∷ String
separator = "."

sign'
  ∷ Secret
  → String
  → Effect String
sign' secret plain = (\h → plain <> separator <> h) <$> hmac secret plain

unsign'
  ∷ Secret
  → String
  → ExceptT String Effect String
unsign' secret signed = do
  parts ← ExceptT $ pure $ note "Incorrect format" $ do
    index ← lastIndexOf (Pattern separator) signed
    let parts = splitAt index signed
    signature ← stripPrefix (Pattern separator) parts.after
    pure { plain: parts.before, signature }
  signature' ← liftEffect $ hmac secret parts.plain
  if parts.signature /= signature'
    then
      throwError "Bad signature"
    else
      pure parts.plain
