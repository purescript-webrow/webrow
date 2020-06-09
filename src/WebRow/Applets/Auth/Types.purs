module WebRow.Applets.Auth.Types where

import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Prelude (class Show)
import Type.Prelude (SProxy(..))

_auth = SProxy ∷ SProxy "auth"

type Namespace t r = ( auth ∷ t | r )

namespace ∷ ∀ a r. a → Variant (Namespace a r)
namespace = inj _auth

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
derive newtype instance showPassword ∷ Show Password

