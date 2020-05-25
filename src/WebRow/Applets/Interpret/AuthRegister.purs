module WebRow.Applets.Interpret.AuthRegister where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Selda (lit, restrict, selectFrom, (.==))
import Selda as Selda
import Selda.PG.Class (class MonadSeldaPG, query1)
import WebRow.Applets.Registration.Types (Password(..))
import WebRow.Crypto as Crypto
import WebRow.Mailer (Email(..))

type UserRow = ( email ∷ String, password ∷ String, salt ∷ String )

usersTable ∷ Selda.Table UserRow
usersTable = Selda.Table { name: "users" }

authenticate
  ∷ ∀ m
  . MonadSeldaPG m
  ⇒ Email
  → Password
  → m (Maybe Unit)
authenticate (Email email) (Password password) = do
  getUserByEmail >>= maybe
    (pure Nothing)
    \user → do
      hashedPass ← liftEffect $ Crypto.hash $ password <> user.salt
      pure $ if user.password == hashedPass
        then Just unit
        else Nothing
  where
    getUserByEmail ∷ m (Maybe { | UserRow })
    getUserByEmail = query1 $ selectFrom usersTable \r → do
      restrict $ r.email .== lit email
      pure r

-- getObjectOr404 ∷ Table t → ({ | cols } → k) → k
