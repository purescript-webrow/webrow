module WebRow.Testing.Messages where

import Prelude

import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder

type Printers =
  ( invalidEmailFormat ∷ String → String )

printers ∷ { | Printers }
printers =
  { invalidEmailFormat:
      \value → "Given value is not a valid email address: " <> show value
  }

print
  ∷ ∀ r r' r''
  . Row.Union r Printers r'
  ⇒ Row.Nub r' r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
print = Record.Builder.merge printers

