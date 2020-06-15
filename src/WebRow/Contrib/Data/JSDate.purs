module WebRow.Contrib.Data.JSDate where

import Data.DateTime.Instant (Instant)
import Data.JSDate (JSDate, fromInstant)
import Unsafe.Coerce (unsafeCoerce)

epoch ∷ JSDate
epoch = fromInstant (unsafeCoerce 0 ∷ Instant)
