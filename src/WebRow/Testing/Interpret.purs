module WebRow.Testing.Interpret where

import Prelude
import Data.List (List)
import Data.List (singleton) as List
import Data.Tuple (Tuple)
import JS.Unsafe.Stringify (unsafeStringify)
import Run (Run)
import Run (interpret, on, send) as Run
import Run.Writer (Writer, runWriterAt, tellAt)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.Mailer (MAILER, Mail, Mailer(..), _mailer)
import WebRow.Message (MESSAGE, Message(..), _message)

handleMessage ∷ ∀ m msgs. Monad m ⇒ Message msgs ~> m
handleMessage (Message v next) = pure $ next (unsafeStringify v)

runMessage ∷ ∀ eff msg. Run (MESSAGE msg + eff) ~> Run eff
runMessage = Run.interpret (Run.on _message handleMessage Run.send)

_mailQueue = Proxy ∷ Proxy "mailQueue"

type MailQueue mails eff
  = ( mailQueue ∷ Writer (List (Mail mails)) | eff )

handleMail ∷ ∀ eff mails. Mailer mails ~> Run (MailQueue mails + eff)
handleMail (Send mail next) = do
  tellAt _mailQueue (List.singleton mail)
  pure next

runMailer ∷ ∀ eff mails. Run (MAILER mails + MailQueue mails + eff) ~> Run (MailQueue mails + eff)
runMailer = Run.interpret (Run.on _mailer handleMail Run.send)

runMailer' ∷ ∀ a eff mails. Run (MAILER mails + MailQueue mails + eff) a → Run (eff) (Tuple (List (Mail mails)) a)
runMailer' = runWriterAt _mailQueue <<< runMailer
