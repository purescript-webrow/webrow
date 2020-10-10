module WebRow.Crypto.Types where

import Data.Argonaut (Json)
import Data.Newtype (class Newtype)

newtype Secret = Secret String

newtype Signed = Signed String

newtype Unsigned = Unsigned String
derive instance newtypeUnsigned ∷ Newtype Unsigned _

newtype Unverified = Unverified Json
derive instance newtypeUnverified ∷ Newtype Unverified _

