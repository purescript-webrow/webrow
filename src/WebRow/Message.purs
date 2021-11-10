module WebRow.Message where

import Prelude

import Run (Run)
import Run as Run
import Type.Row (type (+))
import Type.Prelude (Proxy(..))

data Message msg a
  = Message msg (String → a)

derive instance Functor (Message info)

_message = Proxy ∷ Proxy "message"

type MESSAGE messages eff
  = ( message ∷ Message messages | eff )

message ∷
  ∀ eff msg.
  msg →
  Run (MESSAGE msg + eff) String
message msg = Run.lift _message (Message msg identity)

run ∷ ∀ a eff msg. (msg → String) → Run (MESSAGE msg + eff) a -> Run eff a
run print = Run.interpret (Run.on _message handleMessage Run.send)
  where
    handleMessage ∷ ∀ m. Monad m ⇒ Message msg ~> m
    handleMessage (Message v next) = pure $ next (print v)

