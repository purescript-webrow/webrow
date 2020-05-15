module WebRow.Applets.Auth.Types where

import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))

_auth = SProxy ∷ SProxy "auth"

type Namespace t r = ( auth ∷ t | r )

namespace ∷ ∀ a r. a → Variant (Namespace a r)
namespace = inj _auth
