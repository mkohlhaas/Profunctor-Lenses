-- | This module defines the `Tagged` profunctor
module Data.Lens.Internal.Tagged where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Closed (class Closed)
import Data.Profunctor.Costrong (class Costrong)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

newtype Tagged ∷ ∀ k. k → Type → Type
newtype Tagged a b = Tagged b

derive instance Newtype (Tagged a b) _

derive instance Eq b ⇒ Eq (Tagged a b)
instance Eq1 (Tagged a) where
  eq1 = eq

derive instance Ord b ⇒ Ord (Tagged a b)
instance Ord1 (Tagged a) where
  compare1 = compare

derive instance Functor (Tagged a)

instance Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)

instance Choice Tagged where
  left (Tagged x) = Tagged (Left x)
  right (Tagged x) = Tagged (Right x)

instance Costrong Tagged where
  unfirst (Tagged (Tuple b _)) = Tagged b
  unsecond (Tagged (Tuple _ c)) = Tagged c

instance Closed Tagged where
  closed (Tagged b) = Tagged (const b)

instance Foldable (Tagged a) where
  foldMap f (Tagged a) = f a
  foldr f b (Tagged a) = f a b
  foldl f b (Tagged a) = f b a

instance Traversable (Tagged a) where
  sequence (Tagged a) = map Tagged a
  traverse f (Tagged a) = map Tagged (f a)
