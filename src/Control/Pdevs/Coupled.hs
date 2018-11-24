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
    ) where

import           Control.Lens.Getter
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Pdevs.Component
import           Control.Pdevs.Coupled.Class

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
