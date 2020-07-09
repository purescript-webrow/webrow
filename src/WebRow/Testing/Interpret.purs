module WebRow.Testing.Interpret where

import Prelude

import Global.Unsafe (unsafeStringify)
import Run (Run)
import Run (interpret, on, send) as Run
import Type.Row (type (+))
import WebRow.Message (Message, MessageF(..), _message)

handleMessage ∷ ∀ m msgs. Monad m ⇒ MessageF msgs ~> m
handleMessage (MessageF v next) = pure $ next (unsafeStringify v)

runMessage ∷ ∀ eff msg . Run (Message msg + eff) ~> Run eff
runMessage = Run.interpret (Run.on _message handleMessage Run.send)
