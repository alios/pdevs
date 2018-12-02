{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Control.Pdevs.Coupled.Class
    ( MonadCoupled (..)
    , mkSimulator
    , bindInput, bindOutput, bindIntern
    ) where

import           Control.Lens.Operators    ((^.))
import           Control.Pdevs.AtomicModel
import           Control.Pdevs.Component

class (Monad m) => MonadCoupled m t (d :: [*]) x y | m -> t, m -> d, m -> x, m -> y where
  nextComponentPath :: m ComponentPath
  addBinding :: Z t d x y -> m ()

mkSimulator :: (MonadCoupled m t d x y, HasAtomicModel am t s x' y')
            => am -> s -> m (Component t d x' y')
mkSimulator m s = do
  i <- nextComponentPath
  pure . Simulator i (m ^. atomicModel) $ s

bindInput :: (MonadCoupled m t d x y)
          => Component t d x' y' -> (x -> x') -> m ()
bindInput c = addBinding . BindInput c

bindOutput :: (MonadCoupled m t d x y)
           => Component t d x' y' -> (y' -> y) -> m ()
bindOutput c = addBinding . BindOutput c

bindIntern :: (MonadCoupled m t d x y)
           => Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> m ()
bindIntern a b = addBinding . BindIntern a b
