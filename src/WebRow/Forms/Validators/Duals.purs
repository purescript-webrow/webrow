module WebRow.Forms.Validators.Duals where

import Prelude

import Data.Newtype (unwrap)
import Polyform.Batteries (Dual) as Batteries
import Polyform.Dual (dual) as Dual
import WebRow.Forms.Validators (email) as Validators
import WebRow.Mailer (Email)

email ∷ ∀ e m. Monad m ⇒ Batteries.Dual m (invalidEmailFormat ∷ String | e) String Email
email = Dual.dual Validators.email (unwrap >>> pure)

