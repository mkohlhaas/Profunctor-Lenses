module Data.Lens.Internal.Grating where

import Prelude

import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Closed (class Closed)

newtype Grating a b s t = Grating (((s → a) → b) → t)

derive instance Newtype (Grating a b s t) _

instance Profunctor (Grating a b) where
  dimap f g (Grating z) = Grating \d → g (z \k → d (k <<< f))

instance Closed (Grating a b) where
  closed (Grating z) = Grating \f x → z \k → f \g → k (g x)
