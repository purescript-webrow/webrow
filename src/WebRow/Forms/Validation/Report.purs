module WebRow.Forms.Validation.Report where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Foreign.Object (Object) as Foreign
import WebRow.Utils.Foreign.Object.Builder (Builder) as Foreign.Object

-- | TODO: Use Variant as for errors representation
type Result result = Either (Array String) result

type Key = String

-- | It is `Foreign.Object` and not `Data.Map` because
-- | we have simple builder for it and we do a lot of
-- | "merges" during standard validation flow.
type Report result = Foreign.Object (Result result)

type Builder result = Foreign.Object.Builder (Result result)
