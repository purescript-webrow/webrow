module WebRow.Registration.Interpret.Dummy where

import Prelude

import Data.Either (Either(..))
import Run (Run)
import Run (interpret, on, send) as Run
import WebRow.Applets.Registration (RegisterF(..), REGISTER, _register)

interpret :: forall t15 t7.
   Run
     ( register :: REGISTER
     | t15
     )
     t7
   -> Run t15 t7
interpret = Run.interpret (Run.on _register interpreter Run.send)

interpreter
  ∷ ∀ eff
  . RegisterF ~> Run eff
interpreter (CheckIfEmailRegistered email next) = pure (next (Right email))

