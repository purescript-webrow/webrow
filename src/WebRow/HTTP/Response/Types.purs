module WebRow.HTTP.Response.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
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

data ContentDisposition
  = Inline
  | Attachment (Maybe String)
derive instance eqContentDisposition ∷ Eq ContentDisposition
derive instance ordContentDisposition ∷ Ord ContentDisposition
derive instance genericContentDisposition ∷ Generic ContentDisposition _
instance showContentDisposition ∷ Show ContentDisposition where
  show cd = genericShow cd
