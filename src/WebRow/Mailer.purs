module WebRow.Mailer where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains) as String
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run
import Type.Row (type (+))

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _
derive instance eqEmail ∷ Eq Email
derive instance ordEmail ∷ Ord Email
derive newtype instance showEmail ∷ Show Email

-- | TODO: Fix this smart constructor - possibly use this:
-- | https://github.com/cdepillabout/purescript-email-validate
email ∷ String → Maybe Email
email e = if String.contains (String.Pattern "@") e
  then Just (Email e)
  else Nothing

type Mail mail = { to ∷ Email, context ∷ Variant mail }

data MailerF mails a = SendF (Mail mails) a

derive instance functorMailerF ∷ Functor (MailerF mails)

type MAILER mails = FProxy (MailerF mails)

type Mailer mails eff = (mailer ∷ MAILER mails | eff)

_mailer = SProxy ∷ SProxy "mailer"

send ∷
  ∀ eff mails
  . Mail mails
  → Run (Mailer mails + eff) Unit
send mail = Run.lift _mailer (SendF mail unit)

