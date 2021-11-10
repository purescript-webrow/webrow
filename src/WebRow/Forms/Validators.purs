module WebRow.Forms.Validators where

import Prelude
import Polyform.Batteries (Validator, error) as Batteries
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.Mailer (Email)
import WebRow.Mailer (email) as Mailer

type Messages msgs
  = (InvalidEmailFormat + msgs)

_invalidEmailFormat = Proxy ∷ Proxy "invalidEmailFormat"

type InvalidEmailFormat r
  = ( invalidEmailFormat ∷ String | r )

email ∷ ∀ e m. Monad m ⇒ Batteries.Validator m ( invalidEmailFormat ∷ String | e ) String Email
email =
  Validator.liftFnMaybe
    (Batteries.error _invalidEmailFormat msg)
    Mailer.email
  where
  msg = append "Invalid email format: "
