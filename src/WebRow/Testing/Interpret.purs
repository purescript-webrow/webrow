module WebRow.Testing.Interpret where

import Prelude
import Data.List (List)
import Data.List (singleton) as List
import Data.Tuple (Tuple)
import Global.Unsafe (unsafeStringify)
import Run (Run)
import Run (interpret, on, send) as Run
import Run.Writer (WRITER, runWriterAt, tellAt)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Mailer (Mail, MailerF, Mailer, _mailer)
import WebRow.Mailer (MailerF(..)) as Mailer
import WebRow.Message (Message, MessageF(..), _message)

handleMessage ∷ ∀ m msgs. Monad m ⇒ MessageF msgs ~> m
handleMessage (MessageF v next) = pure $ next (unsafeStringify v)

runMessage ∷ ∀ eff msg. Run (Message msg + eff) ~> Run eff
runMessage = Run.interpret (Run.on _message handleMessage Run.send)

_mailQueue = SProxy ∷ SProxy "mailQueue"

type MailQueue mails eff
  = ( mailQueue ∷ WRITER (List (Mail mails)) | eff )

handleMail ∷ ∀ eff mails. MailerF mails ~> Run (MailQueue mails + eff)
handleMail (Mailer.SendF mail next) = do
  tellAt _mailQueue (List.singleton mail)
  pure next

runMailer ∷ ∀ eff mails. Run (Mailer mails + MailQueue mails + eff) ~> Run (MailQueue mails + eff)
runMailer = Run.interpret (Run.on _mailer handleMail Run.send)

runMailer' ∷ ∀ a eff mails. Run (Mailer mails + MailQueue mails + eff) a → Run (eff) (Tuple (List (Mail mails)) a)
runMailer' = runWriterAt _mailQueue <<< runMailer
