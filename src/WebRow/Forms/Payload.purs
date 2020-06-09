module WebRow.Forms.Payload where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..)) as String
import Data.String (split) as Data.String
import Polyform.Batteries.UrlEncoded.Query (Key, lookup) as UrlDecoded.Query
import Polyform.Batteries.UrlEncoded.Query (parse, Value, Decoded) as UrlEncoded.Query
import Run (Run)
import WebRow.Reader (READER, request)
import WebRow.Response (RESPONSE, badRequest'')

-- | TODO: Use proper reexport here
-- | Array String
type Value = UrlEncoded.Query.Value
type Key = UrlDecoded.Query.Key
-- | Map String (Array String)
type UrlDecoded = UrlEncoded.Query.Decoded
lookup ∷ String → UrlDecoded → Maybe (Array String)
lookup = UrlDecoded.Query.lookup

-- | TODO: Where should we handle logging of errors like urldata decoding problems?

fromQuery ∷ ∀ ctx eff res. Run (reader ∷ READER ctx, response ∷ RESPONSE res | eff) UrlDecoded
fromQuery
  = maybe badRequest'' pure
  <<< join
  <<< map (UrlEncoded.Query.parse { replacePlus: true })
  <<< queryString
  <<< _.url
  =<< request
  where
    queryString fullPath = case Data.String.split (String.Pattern "?") fullPath of
      [_, q] → Just q
      otherwise → Nothing

fromBody ∷ ∀ ctx eff res. Run (reader ∷ READER ctx, response ∷ RESPONSE res | eff) UrlDecoded
fromBody = do
  possiblyDecoded ← request <#> (UrlEncoded.Query.parse { replacePlus: true } <<< _.body)
  maybe badRequest'' pure possiblyDecoded
