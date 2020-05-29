module WebRow.Applets.Interpret.AuthRegister where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Selda (Col, lit, restrict, selectFrom, (.==))
import Selda as Selda
import Selda.PG.Class (class MonadSeldaPG, BackendPGClass, query1)
import Selda.Query (class FromTable)
import Selda.Query.Class (class GenericQuery)
import WebRow.Applets.Registration.Types (Password(..))
import WebRow.Crypto as Crypto
import WebRow.Mailer (Email(..))

type UserRow r = ( email ∷ String, password ∷ String, salt ∷ String | r )

usersTable ∷ Selda.Table (UserRow ())
usersTable = Selda.Table { name: "users" }

authenticate
  ∷ ∀ m
  . MonadSeldaPG m
  ⇒ Email
  → Password
  → m (Maybe Unit)
authenticate (Email email) (Password password) = do
  getUserByEmail usersTable email >>= maybe
    (pure Nothing)
    \user → do
      hashedPass ← liftEffect $ Crypto.hash $ password <> user.salt
      pure $ if user.password == hashedPass
        then Just unit
        else Nothing

getUserByEmail
  ∷ ∀ t m i o
  . MonadSeldaPG m
  ⇒ FromTable Unit t ( email ∷ Col Unit String | i )
  ⇒ GenericQuery BackendPGClass m 
      ( email ∷ Col Unit String | i )
      ( email ∷ String | o )
  ⇒ Selda.Table t → String → m (Maybe { email ∷ String | o })
getUserByEmail table email = query1 q
  where
    q = selectFrom table \r → do
      restrict $ r.email .== lit email
      pure r

