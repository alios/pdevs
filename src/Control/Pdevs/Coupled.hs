{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Control.Pdevs.Coupled
    ( module Control.Pdevs.Coupled.Class
    , CoupledT, mkCoordinator
    , RootCoordinator, mkRootCoordinator
    , ComponentPath, HasComponentPaths(..)
    ) where

import           Control.Lens.Getter
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Lens.Iso
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Pdevs.Component
import           Control.Pdevs.Coupled.Class
import           Data.Vector (Vector)

data CoupledState = CoupledState
    { _coupledStateI    :: Int
    , _coupledStatePath :: ComponentPath
    }
makeClassy ''CoupledState

newtype CoupledT t (d :: [*]) x y m a =
  CoupledT (StateT CoupledState (WriterT (Vector (Z t d x y)) m) a)
  deriving ( Functor, Applicative, Monad
           , MonadState CoupledState, MonadWriter (Vector (Z t d x y))
           )

_CoupledState :: Iso' CoupledState ComponentPath
_CoupledState = iso (view coupledStatePath) (CoupledState 0)

instance (Monad m) => MonadCoupled (CoupledT t d x y m) t d x y where
  nextComponentPath = do
    i <- use coupledStateI
    coupledStateI += 1
    (<|) <$> pure i <*> use coupledStatePath
  addBinding = tell . pure

instance MonadTrans (CoupledT t d x y) where
  lift = CoupledT . lift . lift

runCoupledT :: Monad m => ComponentPath -> CoupledT t d x y m a -> m (Vector (Z t d x y))
runCoupledT p (CoupledT m) = snd <$> runWriterT (evalStateT m (_CoupledState # p))

mkCoordinator :: Monad m => CoupledT t (d' : d) x' y' m () -> CoupledT t d x y m (Component t d x' y')
mkCoordinator m = do
  p <- nextComponentPath
  c <- lift $ runCoupledT p m
  return $ Coordinator p c

mkRootCoordinator
  :: Monad m
  => CoupledT t '[] x y m (Component t '[] x y) -> m (RootCoordinator t x y)
mkRootCoordinator m = fmap RootCoordinator . runCoupledT mempty $ do
  rc <- m
  bindInput rc id
  bindOutput rc id
