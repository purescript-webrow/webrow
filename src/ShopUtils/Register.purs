module ShopUtils.Register where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Run (Run(..), AFF)
import Run.Reader (READER)
import Unsafe.Coerce (unsafeCoerce)

data Route
  = RegisterEmail Email
  | RegisterPassword { email ∷ SignedEmail, password ∷ Maybe Password }
derive instance genericRoute ∷ Generic Route _

route ∷ D.RouteDuplex' Route
route = D.path "register" $ DG.sum
  { "RegisterEmail": (_Newtype $ D.param "email" ∷ D.RouteDuplex' Email)
  , "RegisterPassword": D.params
      { email: _Newtype <<< D.string
      , password: D.optional <<< _Newtype <<< D.string
      }
  }

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _

newtype SignedEmail = SignedEmail String
derive instance newtypeSignedEmail ∷ Newtype SignedEmail _

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _

checkIfTaken ∷ ∀ m. Email → m Unit
checkIfTaken = unsafeCoerce unit

sign
  ∷ ∀ eff ctx
  . String
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ String | ctx } | eff
      )
      String
sign = unsafeCoerce unit

type MAILER = Unit

sendMail
  ∷ ∀ eff
  . { to ∷ Email, body ∷ String }
  → Run ( mailer ∷ MAILER | eff ) Unit
sendMail = unsafeCoerce unit

registerEmail email = do
  -- validateEmail email
  checkIfTaken email -- either actual db function with concrete SQL table or a function of RegisterEffect
  signedEmail ← sign $ un Email email
  let body = D.print route $ RegisterPassword { email: SignedEmail signedEmail, password: Nothing } 
  sendMail { to: email, body }
  pure "Email sent"
