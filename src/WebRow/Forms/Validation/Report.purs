module WebRow.Forms.Validation.Report where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign.Object (Object) as Foreign
import WebRow.Forms.Payload (Value)
import WebRow.Utils.Foreign.Object.Builder (Builder) as Foreign.Object

type Result result =
  { result ∷ Maybe (Either (Array String) result)
  , input ∷ Maybe Value
  }

type Key = String

-- | It is `Foreign.Object` and not `Data.Map` because
-- | we have simple builder for it and we do a lot of
-- | "mergings" during standard validation flow.
type Report result = Foreign.Object (Result result)

type Builder result = Foreign.Object.Builder (Result result)
