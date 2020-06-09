module WebRow.Applets.Registration.Types where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _
derive newtype instance showSignedEmail ∷ Show SignedEmail

_register = SProxy ∷ SProxy "register"

-- IMO these type parameters should be flipped to allow easy namespace appending
type Namespace r t = (register ∷ t | r)

namespace ∷ ∀ a r. a → Variant (Namespace r a)
namespace = inj _register
