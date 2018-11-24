{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Control.Lens.Getter    (use, view)
import           Control.Lens.Operators
import           Control.Lens.TH        (makeClassy, makePrisms)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Vector            (Vector)

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

--
-- Components
--

type ComponentPath  = [Int]

data Component t (d :: [*]) x y where
  Simulator :: ComponentPath -> AtomicModel t s x y -> s -> Component t d x y

data Z t d x y where
  BindInput  :: Component t d x' y' -> (x -> x')  -> Z t d x y
  BindOutput :: Component t d x' y' -> (y' -> y) -> Z t d x y
  BindIntern :: Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> Z t d x y

data Simulation t x y

--
-- MonadCoupled
--

class (Monad m) => MonadCoupled m t (d :: [*]) x y | m -> t, m -> d, m -> x, m -> y where
  nextComponentPath :: m ComponentPath
  addBinding :: Z t d x y -> m ()

mkSimulator :: (MonadCoupled m t d x y, HasAtomicModel am t s x' y')
            => am -> s -> m (Component t d x' y')
mkSimulator m s = do
  i <- nextComponentPath
  pure . Simulator i (view atomicModel m) $ s

bindInput :: (MonadCoupled m t d x y)
          => Component t d x' y' -> (x -> x') -> m ()
bindInput c = addBinding . BindInput c

bindOutput :: (MonadCoupled m t d x y)
           => Component t d x' y' -> (y' -> y) -> m ()
bindOutput c = addBinding . BindOutput c

bindIntern :: (MonadCoupled m t d x y)
           => Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> m ()
bindIntern a b = addBinding . BindIntern a b


--
-- CoupledT
--

data CoupledState = CoupledState
    { _coupledStatePath :: ComponentPath
    , _coupledStateI    :: Int
    }
makeClassy ''CoupledState

newtype CoupledT t (d :: [*]) x y m a =
  CoupledT (StateT CoupledState (WriterT [Z t d x y] m) a)
  deriving ( Functor, Applicative, Monad
           , MonadState CoupledState, MonadWriter [Z t d x y]
           )

instance (Monad m) => MonadCoupled (CoupledT t d x y m) t d x y where
  nextComponentPath = do
    i <- use coupledStateI
    coupledStateI += 1
    (:) <$> pure i <*> use coupledStatePath
  addBinding = tell . pure

mkCoordinator :: Monad m => CoupledT t (d':d) x' y' m () -> CoupledT t d x y m (Component t d x' y')
mkCoordinator m = undefined

mkRootCoordinator :: Monad m => CoupledT t '[] x y m (Component t '[] x' y') -> m (Simulation t x' y')
mkRootCoordinator = undefined

--
-- Examples / Testing
--

m1 :: AtomicModel Float () Int Double
m1 = defaultAtomicModel

t1 :: Monad m
   => CoupledT Float d x y m (Component Float d Int String)
t1 = mkCoordinator $ do
  s0 <- mkSimulator m1 ()
  c1 <- mkCoordinator $ pure ()

  c0 <- mkCoordinator $ do
    s1 <- mkSimulator m1 ()
    bindInput s1 id
    bindOutput s1 show
  bindIntern s0 c0 round
  bindInput s0 id
  bindOutput c0 id
