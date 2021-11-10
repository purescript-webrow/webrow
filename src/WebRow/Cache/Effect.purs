module WebRow.Cache.Effect where

import Prelude
import Data.Maybe (Maybe)

import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Type.Prelude (class IsSymbol)
import Type.Row (type (+))
import Type.Prelude (Proxy(..))

type Key
  = String

data Cache attrs value a
  = DeleteF Key a
  | LookupF Key (Maybe value → a)
  | InsertF Key attrs value (Boolean → a)

derive instance functorDataStoreF ∷ Functor (Cache attrs val)

type CACHE attrs v eff
  = ( cache ∷ Cache attrs v | eff )

liftCacheAt ∷
  ∀ a attrs eff eff_ s v.
  IsSymbol s ⇒
  Row.Cons s (Cache attrs v) eff_ eff ⇒
  Proxy s →
  Cache attrs v a →
  Run eff a
liftCacheAt = Run.lift

_cache = Proxy ∷ Proxy "cache"

deleteAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (Cache attrs v) eff_ eff ⇒
  (Proxy l) →
  Key →
  Run eff Unit
deleteAt l key = liftCacheAt l (DeleteF key unit)

delete ∷
  ∀ attrs eff v.
  Key →
  Run (CACHE attrs v + eff) Unit
delete = deleteAt _cache

lookupAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (Cache attrs v) eff_ eff ⇒
  Proxy l →
  Key →
  Run eff (Maybe v)
lookupAt l key = liftCacheAt l (LookupF key identity)

lookup ∷
  ∀ attrs eff v.
  Key →
  Run (CACHE attrs v + eff) (Maybe v)
lookup = lookupAt _cache

insertAt ∷
  ∀ attrs eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (Cache attrs v) eff_ eff ⇒
  Proxy l →
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
  Run (CACHE attrs v + eff) Boolean
insert = insertAt _cache

