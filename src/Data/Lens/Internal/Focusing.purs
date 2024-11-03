-- | This module defines the `Focusing` functor
module Data.Lens.Internal.Focusing where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

-- | The functor used to zoom into `StateT`.
newtype Focusing m s a = Focusing (m (Tuple s a))

derive instance Newtype (Focusing m s a) _

instance Functor m ⇒ Functor (Focusing m s) where
  map f (Focusing r) = Focusing (map (map f) r)

instance (Apply m, Semigroup s) ⇒ Apply (Focusing m s) where
  apply (Focusing rf) (Focusing rx) = Focusing (map (<*>) rf <*> rx)

instance (Applicative m, Monoid s) ⇒ Applicative (Focusing m s) where
  pure = Focusing <<< pure <<< pure
