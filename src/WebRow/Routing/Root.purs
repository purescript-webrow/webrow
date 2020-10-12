module WebRow.Routing.Routing.Root where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Prim.Row (class Lacks) as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder, build, insert) as Record.Builder
import Routing.Duplex (RouteDuplex', root) as D
import Routing.Duplex.Generic (noArgs, sum) as D
import Routing.Duplex.Generic.Variant (class Variant')
import Routing.Duplex.Generic.Variant (variant') as D

data Root
  = Root

derive instance genericRoot ∷ Generic Root _

_root = SProxy ∷ SProxy ""

type Route routes
  = ( "" ∷ Root | routes )

type Duplex duplexes
  = ( "" ∷ D.RouteDuplex' Root | duplexes )

rootRouteDuplex ∷ D.RouteDuplex' Root
rootRouteDuplex = D.sum { "Root": D.noArgs }

build ∷
  ∀ routes rl duplexes.
  RowToList (Duplex duplexes) rl ⇒
  Variant' rl (Duplex duplexes) routes ⇒
  Row.Lacks "" duplexes ⇒
  Record.Builder.Builder {} (Record duplexes) →
  D.RouteDuplex' (Variant routes)
build routesBuilder =
  D.root $ D.variant'
    $ Record.Builder.build
        (Record.Builder.insert _root rootRouteDuplex <<< routesBuilder)
        {}
