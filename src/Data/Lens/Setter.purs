-- | This module defines functions for working with setters.
module Data.Lens.Setter
  ( (%~)
  , over
  , iover
  , (.~)
  , set
  , (+~)
  , addOver
  , (-~)
  , subOver
  , (*~)
  , mulOver
  , (//~)
  , divOver
  , (||~)
  , disjOver
  , (&&~)
  , conjOver
  , (<>~)
  , appendOver
  , (?~)
  , setJust
  , (.=)
  , assign
  , (%=)
  , modifying
  , (+=)
  , addModifying
  , (*=)
  , mulModifying
  , (-=)
  , subModifying
  , (//=)
  , divModifying
  , (||=)
  , disjModifying
  , (&&=)
  , conjModifying
  , (<>=)
  , appendModifying
  , (?=)
  , assignJust
  , module Data.Lens.Types
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState, modify)
import Data.Lens.Types (Indexed(..), IndexedSetter, Setter, Setter')
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)

infixr 4 over as %~
infixr 4 set as .~
infixr 4 addOver as +~
infixr 4 subOver as -~
infixr 4 mulOver as *~
infixr 4 divOver as //~
infixr 4 disjOver as ||~
infixr 4 conjOver as &&~
infixr 4 appendOver as <>~
infixr 4 setJust as ?~

infix 4 assign as .=
infix 4 modifying as %=
infix 4 addModifying as +=
infix 4 mulModifying as *=
infix 4 subModifying as -=
infix 4 divModifying as //=
infix 4 disjModifying as ||=
infix 4 conjModifying as &&=
infix 4 appendModifying as <>=
infix 4 assignJust as ?=

-- | Apply a function to the foci of a `Setter`.
over ∷ ∀ s t a b. Setter s t a b → (a → b) → s → t
over l = l

-- | Apply a function to the foci of a `Setter` that may vary with the index.
iover ∷ ∀ i s t a b. IndexedSetter i s t a b → (i → a → b) → s → t
iover l f = l (Indexed $ uncurry f)

-- | Set the foci of a `Setter` to a constant value.
set ∷ ∀ s t a b. Setter s t a b → b → s → t
set l b = over l (const b)

addOver ∷ ∀ s t a. Semiring a ⇒ Setter s t a a → a → s → t
addOver p = over p <<< add

mulOver ∷ ∀ s t a. Semiring a ⇒ Setter s t a a → a → s → t
mulOver p = over p <<< flip mul

subOver ∷ ∀ s t a. Ring a ⇒ Setter s t a a → a → s → t
subOver p = over p <<< flip sub

divOver ∷ ∀ s t a. EuclideanRing a ⇒ Setter s t a a → a → s → t
divOver p = over p <<< flip div

disjOver ∷ ∀ s t a. HeytingAlgebra a ⇒ Setter s t a a → a → s → t
disjOver p = over p <<< flip disj

conjOver ∷ ∀ s t a. HeytingAlgebra a ⇒ Setter s t a a → a → s → t
conjOver p = over p <<< flip conj

appendOver ∷ ∀ s t a. Semigroup a ⇒ Setter s t a a → a → s → t
appendOver p = over p <<< flip append

setJust ∷ ∀ s t a b. Setter s t a (Maybe b) → b → s → t
setJust p = set p <<< Just

-- Stateful

-- | Set the foci of a `Setter` in a monadic state to a constant value.
assign ∷ ∀ s a b m. MonadState s m ⇒ Setter s s a b → b → m Unit
assign p b = void (modify (set p b))

-- | Modify the foci of a `Setter` in a monadic state.
modifying ∷ ∀ s a b m. MonadState s m ⇒ Setter s s a b → (a → b) → m Unit
modifying p f = void (modify (over p f))

addModifying ∷ ∀ s a m. MonadState s m ⇒ Semiring a ⇒ Setter' s a → a → m Unit
addModifying p = modifying p <<< add

mulModifying ∷ ∀ s a m. MonadState s m ⇒ Semiring a ⇒ Setter' s a → a → m Unit
mulModifying p = modifying p <<< flip mul

subModifying ∷ ∀ s a m. MonadState s m ⇒ Ring a ⇒ Setter' s a → a → m Unit
subModifying p = modifying p <<< flip sub

divModifying ∷ ∀ s a m. MonadState s m ⇒ EuclideanRing a ⇒ Setter' s a → a → m Unit
divModifying p = modifying p <<< flip div

disjModifying ∷ ∀ s a m. MonadState s m ⇒ HeytingAlgebra a ⇒ Setter' s a → a → m Unit
disjModifying p = modifying p <<< flip disj

conjModifying ∷ ∀ s a m. MonadState s m ⇒ HeytingAlgebra a ⇒ Setter' s a → a → m Unit
conjModifying p = modifying p <<< flip conj

appendModifying ∷ ∀ s a m. MonadState s m ⇒ Semigroup a ⇒ Setter' s a → a → m Unit
appendModifying p = modifying p <<< flip append

assignJust ∷ ∀ s a b m. MonadState s m ⇒ Setter s s a (Maybe b) → b → m Unit
assignJust p = assign p <<< Just
