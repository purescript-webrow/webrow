module WebRow.Forms.Payload where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..)) as String
import Data.String (split) as Data.String
import Polyform.Batteries.UrlEncoded.Query (Key, lookup) as UrlDecoded.Query
import Polyform.Batteries.UrlEncoded.Query (parse, Value, Decoded) as UrlEncoded.Query
import Run (Run)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, badRequest'')
import WebRow.Request (Request, body, fullPath)

type Value = UrlEncoded.Query.Value
type Key = UrlDecoded.Query.Key
-- | Map String (Array String)
type UrlDecoded = UrlEncoded.Query.Decoded
lookup ∷ String → UrlDecoded → Maybe (Array String)
lookup = UrlDecoded.Query.lookup

-- | TODO: Where should we handle logging of errors like urldata decoding problems?

fromQuery ∷ ∀ eff. Run (Request + HTTPExcept + eff) UrlDecoded
fromQuery
  = maybe badRequest'' pure
  <<< join
  <<< map (UrlEncoded.Query.parse { replacePlus: true })
  <<< queryString
  =<< fullPath
  where
    queryString fullPath = case Data.String.split (String.Pattern "?") fullPath of
      [_, q] → Just q
      otherwise → Nothing

fromBody ∷ ∀ eff. Run (Request + HTTPExcept + eff) UrlDecoded
fromBody = do
  bodyStr ← body
  maybe badRequest'' pure (UrlEncoded.Query.parse { replacePlus: true } bodyStr)
