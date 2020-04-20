module ShopUtils.Types where

import Data.Newtype (class Newtype)

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
