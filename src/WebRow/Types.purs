module WebRow.Types where

import Prelude

import Data.Newtype (class Newtype)

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _
derive newtype instance showEmail ∷ Show Email

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _
derive newtype instance showSignedEmail ∷ Show SignedEmail

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
derive newtype instance showPassword ∷ Show Password
