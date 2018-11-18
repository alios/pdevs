{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Lib where

import           Control.Lens.Getter    (view)
import           Control.Lens.Operators (( # ))
import           Control.Lens.Review    (review)
import           Control.Lens.TH        (makeClassy, makePrisms)
import           Data.Vector            (Vector)
import           Data.Void              (Void)
import           GHC.TypeLits

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
  _AtomicModel # (id, constState, constState, const mempty, const (1/0) )
  where constState s _ = const s


data Component t (d :: [*]) x y

data Simulation t x y

newtype CoupledT t (d :: [*]) x y a =
  CoupledT ()

instance Functor (CoupledT t d x y)
instance Applicative (CoupledT t d x y)
instance Monad (CoupledT t d x y)


mkSimulator :: AtomicModel t s x' y' -> CoupledT t d x y (Component t d x' y')
mkSimulator m = undefined

mkCoordinator :: CoupledT t (d':d) x' y' () -> CoupledT t d x y (Component t d x' y')
mkCoordinator m = undefined

bindInput :: Component t d x' y' -> (x -> x') -> CoupledT t d x y ()
bindInput c f = undefined

bindOutput :: Component t d x' y' -> (y' -> y) -> CoupledT t d x y ()
bindOutput c f = undefined

bindIntern :: Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> CoupledT t d x y ()
bindIntern = undefined

mkRootCoordinator :: CoupledT t '[] x y (Component t '[] x' y') -> Simulation t x' y'
mkRootCoordinator = undefined

m1 :: AtomicModel Float () Int Double
m1 = defaultAtomicModel

t1 = mkRootCoordinator . mkCoordinator $ do
  s0 <- mkSimulator m1
  c1 <- mkCoordinator $ pure ()

  c0 <- mkCoordinator $ do
    s1 <- mkSimulator m1
    bindInput s1 id
    bindOutput s1 show
  bindIntern s0 c0 round
  bindInput s0 id
  bindOutput c0 id
