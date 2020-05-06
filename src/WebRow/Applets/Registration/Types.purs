module WebRow.Applets.Registration.Types where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _
derive newtype instance showSignedEmail ∷ Show SignedEmail

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
derive newtype instance showPassword ∷ Show Password

_register = SProxy ∷ SProxy "register"

type Namespace r t = (register ∷ t | r)

namespace ∷ ∀ a r. a → Variant (Namespace r a)
namespace = inj _register
