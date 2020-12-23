module WebRow.Cache.Effect where

import Prelude
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Type.Prelude (class IsSymbol)
import Type.Row (type (+))

type Key
  = String

data CacheF attrs value a
  = DeleteF Key a
  | LookupF Key (Maybe value → a)
  | InsertF Key attrs value (Boolean → a)

derive instance functorDataStoreF ∷ Functor (CacheF attrs val)

type CACHE attrs v
  = FProxy (CacheF attrs v)

type Cache attrs v eff
  = ( cache ∷ CACHE attrs v | eff )

liftCacheAt ∷
  ∀ a attrs eff eff_ s v.
  IsSymbol s ⇒
  Row.Cons s (CACHE attrs v) eff_ eff ⇒
  SProxy s →
  CacheF attrs v a →
  Run eff a
liftCacheAt = Run.lift

_cache = SProxy ∷ SProxy "cache"

deleteAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (CACHE attrs v) eff_ eff ⇒
  (SProxy l) →
  Key →
  Run eff Unit
deleteAt l key = liftCacheAt l (DeleteF key unit)

delete ∷
  ∀ attrs eff v.
  Key →
  Run (Cache attrs v + eff) Unit
delete = deleteAt _cache

lookupAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (CACHE attrs v) eff_ eff ⇒
  SProxy l →
  Key →
  Run eff (Maybe v)
lookupAt l key = liftCacheAt l (LookupF key identity)

lookup ∷
  ∀ attrs eff v.
  Key →
  Run (Cache attrs v + eff) (Maybe v)
lookup = lookupAt _cache

insertAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (CACHE attrs v) eff_ eff ⇒
  SProxy l →
  Key →
  attrs →
  v →
  Run eff Boolean
insertAt l key attrs val = liftCacheAt l (InsertF key attrs val identity)

insert ∷
  ∀ attrs eff v.
  Key →
  attrs →
  v →
  Run (Cache attrs v + eff) Boolean
insert = insertAt _cache

