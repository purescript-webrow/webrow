module WebRow.Mailer where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains) as String
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _
derive instance eqEmail ∷ Eq Email
derive newtype instance showEmail ∷ Show Email

-- | TODO: Fix this smart constructor - possibly use this:
-- | https://github.com/cdepillabout/purescript-email-validate
email ∷ String → Maybe Email
email e = if String.contains (String.Pattern "@") e
  then Just (Email e)
  else Nothing

data MailerF a
  = SendMail
      { to ∷ Email
      , subject ∷ String
      , text ∷ String
      }
      (String → a)

derive instance functorMailerF ∷ Functor MailerF

type MAILER = FProxy MailerF

type MailerEff eff = ( mailer ∷ MAILER | eff )

_mailer = SProxy ∷ SProxy "mailer"

sendMail ∷
  ∀ eff
  . { to ∷ Email
    , subject ∷ String
    , text ∷ String
    }
  → Run ( mailer ∷ MAILER | eff ) String
sendMail msg = Run.lift _mailer (SendMail msg identity)
