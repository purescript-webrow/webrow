module WebRow.Forms.Payload where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..)) as String
import Data.String (split) as Data.String
import Polyform.Validators.UrlEncoded.Parser (parse) as UrlEncoded.Parser
import Polyform.Validators.UrlEncoded.Types (Value, Decoded) as Polyform.Validators.UrlEncoded.Types
import Run (Run)
import WebRow.Reader (READER, request)
import WebRow.Response (RESPONSE, badRequest'')

-- | Array String
type Value = Polyform.Validators.UrlEncoded.Types.Value

-- | Map String (Array String)
type UrlDecoded = Polyform.Validators.UrlEncoded.Types.Decoded

-- | TODO: Where should we handle logging of errors like urldata decoding problems?

fromQuery ∷ ∀ ctx eff res. Run (reader ∷ READER ctx, response ∷ RESPONSE res | eff) UrlDecoded
fromQuery
  = either (const $ badRequest'') pure
  <<< fromMaybe (Right mempty)
  <<< map (UrlEncoded.Parser.parse { replacePlus: true })
  <<< queryString
  <<< _.url
  =<< request
  where
    queryString fullPath = case Data.String.split (String.Pattern "?") fullPath of
      [_, q] → Just q
      otherwise → Nothing

fromBody ∷ ∀ ctx eff res. Run (reader ∷ READER ctx, response ∷ RESPONSE res | eff) UrlDecoded
fromBody = do
  possiblyDecoded ← request <#> (UrlEncoded.Parser.parse { replacePlus: true } <<< _.body)
  either (const $ badRequest'') pure possiblyDecoded
