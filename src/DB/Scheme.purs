module DB.Scheme where

import Prelude

import Selda.PG.Class (class MonadSeldaPG)
import SeldaUtils (exec)

createTables ∷ ∀ m. MonadSeldaPG m ⇒ m Unit
createTables = exec """
  CREATE TABLE IF NOT EXISTS host_port (
    host text NOT NULL,
    port integer NOT NULL,
    UNIQUE (host, port)
  );


  """
