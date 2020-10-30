module WebRow.HTTP.Response.Types where

import Data.Newtype (class Newtype)
import HTTPure (Headers, Status) as HTTPure
import Node.Buffer (Buffer)
import Node.Stream (Readable) as Stream

data Body = BodyStream (Stream.Readable ()) | BodyBuffer Buffer | BodyString String

type Parts
  = { body ∷ Body, headers ∷ HTTPure.Headers, status ∷ HTTPure.Status }

-- | A tiny wrapper around response which enables
-- | inspection during testing.
newtype HTTPResponse = HTTPResponse Parts

derive instance newtypeHTTPResponse ∷ Newtype HTTPResponse _
