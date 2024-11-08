-- | This module defines the `Wander` type class, which is used to define
-- | `Traversal`s.
module Data.Lens.Internal.Wander where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (alaF)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)

-- | Class for profunctors that support polymorphic traversals.
class (Strong p, Choice p) ⇐ Wander p where
  wander
    ∷ ∀ s t a b
    . (∀ f. Applicative f ⇒ (a → f b) → s → f t)
    → p a b
    → p s t

instance Wander Function where
  wander t = alaF Identity t -- cannot eta-reduce due to ConstrainedTypeUnified error

instance Applicative f ⇒ Wander (Star f) where
  wander t (Star f) = Star (t f)
