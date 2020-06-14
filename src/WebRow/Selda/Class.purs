module WebRow.Selda.Class where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Database.PostgreSQL as PG
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Selda.Query.Class (hoistSeldaWith)

_pgError = SProxy ∷ SProxy "pgError"

class
  ( MonadAff m
  , MonadError (Variant ( pgError ∷ PG.PGError | e )) m
  , MonadReader { conn ∷ PG.Connection | r } m
  ) <= MonadSeldaPG_ m e r | m → e r

instance monadSeldaPG_
  ∷ ( MonadAff m
    , MonadError (Variant ( pgError ∷ PG.PGError | e )) m
    , MonadReader { conn ∷ PG.Connection | r } m
    )
  ⇒ MonadSeldaPG_ m e r

hoistSelda
  ∷ ∀ m e r
  . MonadSeldaPG_ m e r
  ⇒ SeldaPG ~> m
hoistSelda = hoistSeldaWith (inj _pgError) (_.conn)

type SeldaPG = ExceptT PG.PGError (ReaderT PG.Connection Aff)

type SeldaPG_ e r =
  ExceptT
    (Variant ( pgError ∷ PG.PGError | e ))
    (ReaderT { conn ∷ PG.Connection | r } Aff)

