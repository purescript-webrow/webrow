module WebRow.Message where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run

message
  ∷ ∀ eff msg
  . Variant msg
  → Run ( message ∷ MESSAGE msg | eff ) String
message msg = Run.lift _message (MessageF msg identity)

data MessageF msg a = MessageF msg (String → a)
derive instance functorSessionF ∷ Functor (MessageF info)

_message = SProxy ∷ SProxy "message"

type MESSAGE messages = FProxy (MessageF (Variant messages))

type Message messages eff = (message ∷ MESSAGE messages | eff)

-- type Message' messages eff = Message (Variant messages) eff

