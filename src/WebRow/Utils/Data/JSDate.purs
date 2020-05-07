module WebRow.Utils.Data.JSDate where

import Data.JSDate (JSDate, fromInstant)
import Unsafe.Coerce (unsafeCoerce)

epoch ∷ JSDate
epoch = fromInstant (unsafeCoerce 0)
