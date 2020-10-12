module WebRow.HTTP.Response.Types where

import Data.Newtype (class Newtype)
import HTTPure (Headers, Status) as HTTPure

type Parts body
  = { body ∷ body, headers ∷ HTTPure.Headers, status ∷ HTTPure.Status }

-- | A tiny wrapper around response which enables
-- | inspection during testing.
newtype HTTPResponse body
  = HTTPResponse (Parts body)

derive instance newtypeHTTPResponse ∷ Newtype (HTTPResponse body) _
