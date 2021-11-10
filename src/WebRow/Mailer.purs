module WebRow.Mailer where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains) as String
import Data.Variant (Variant)
import Run (Run)
import Run as Run
import Type.Row (type (+))
import Type.Prelude (Proxy(..))

newtype Email
  = Email String

derive instance newtypeEmail ∷ Newtype Email _

derive instance eqEmail ∷ Eq Email

derive instance ordEmail ∷ Ord Email

derive newtype instance showEmail ∷ Show Email

-- | TODO: Fix this smart constructor - possibly use this:
-- | https://github.com/cdepillabout/purescript-email-validate
email ∷ String → Maybe Email
email e =
  if String.contains (String.Pattern "@") e then
    Just (Email e)
  else
    Nothing

type Mail mail
  = { to ∷ Email, context ∷ Variant mail }

data Mailer mails a = Send (Mail mails) a

derive instance functorMailerF ∷ Functor (Mailer mails)

type MAILER mails eff
  = ( mailer ∷ Mailer mails | eff )

_mailer = Proxy ∷ Proxy "mailer"

send ∷
  ∀ eff mails.
  Mail mails →
  Run (MAILER mails + eff) Unit
send mail = Run.lift _mailer (Send mail unit)
