module WebRow.Applets.Registration.Types where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _
derive newtype instance showSignedEmail ∷ Show SignedEmail

_registration = SProxy ∷ SProxy "registration"

type Namespace t r = (registration ∷ t | r)

namespace ∷ ∀ a r. a → Variant (Namespace a r)
namespace = inj _registration
