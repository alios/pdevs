{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Lib
    ( AtomicModel, HasAtomicModel(..), _AtomicModel, defaultAtomicModel
    , CoupledModel, newSimulator, newCoordinator
    , bindInput, bindOutput, bindIntern
    , RootCoordinator
    ) where

import           Control.Lens.Getter    (view)
import           Control.Lens.Operators (( # ))
import           Control.Lens.Review    (review)
import           Control.Lens.TH        (makeClassy, makePrisms)
import           Data.Vector            (Vector)
import           GHC.TypeNats


data AtomicModel t s x y = AtomicModel
  { _deltaInt :: s -> s
  , _deltaExt :: s -> t -> Vector x -> s
  , _deltaCon :: s -> t -> Vector x -> s
  , _lambda   :: s -> Vector y
  , _ta       :: s -> t
  }

makePrisms ''AtomicModel
makeClassy ''AtomicModel

defaultAtomicModel :: (Fractional t) => AtomicModel t s x y
defaultAtomicModel =
  _AtomicModel # (id, \s _ _ -> s,  \s _ _ -> s, const mempty, const (1/0) )

newtype CoupledModel (n :: Nat) t x y a = CoupledModel ()

instance Functor (CoupledModel n t x y)
instance Applicative (CoupledModel n t x y)
instance Monad (CoupledModel n t x y)

data Component (n :: Nat) t x y where
  Simulator       :: AtomicModel t s x y -> Component n t x y
  Coordinator     :: CoupledModel (n + 1) t x y () -> Component n t x y

makeClassy ''Component
makePrisms ''Component

newtype RootCoordinator t x y =
  RootCoordinator (CoupledModel 0 t x y ())

makeClassy ''RootCoordinator
makePrisms ''RootCoordinator

newSimulator :: (KnownNat n, HasAtomicModel c t s' x' y')
             => c -> CoupledModel n t x y (Component n t x' y')
newSimulator = pure . review _Simulator . view atomicModel

newCoordinator :: (KnownNat n)
               => CoupledModel (n + 1) t x' y' ()
               -> CoupledModel n t x y (Component n t x' y')
newCoordinator = pure . review _Coordinator

bindInput :: (KnownNat n)
          => Component n t x' y' -> (x -> x') -> CoupledModel n t x y ()
bindInput = undefined

bindOutput :: (KnownNat n)
           => Component n t x' y' -> (y' -> y) -> CoupledModel n t x y ()
bindOutput = undefined

bindIntern :: (KnownNat n)
           => Component n t x' y' -> Component n t x'' y'' -> (y' -> x'')
           -> CoupledModel n t x y ()
bindIntern = undefined
