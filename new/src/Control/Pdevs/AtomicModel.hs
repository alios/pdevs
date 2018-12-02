{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Control.Pdevs.AtomicModel
  ( AtomicModel, defaultAtomicModel, HasAtomicModel(..)
  ) where

import           Control.Lens.Operators (( # ))
import           Control.Lens.TH
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
