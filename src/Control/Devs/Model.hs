{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE Trustworthy            #-}

module Control.Devs.Model
     ( T, DeltaInt, _DeltaInt, DeltaExt, _DeltaExt, DeltaCon, _DeltaCon
     , Lambda, _Lambda, Ta, _Ta, Model(..), HasModel(..)
     ) where

import           Control.Lens
import           Data.Vector  (Vector)

type T = Double

newtype DeltaInt s = DeltaInt (s -> s)
makePrisms ''DeltaInt

newtype DeltaExt s x = DeltaExt ((s, T) -> Vector x -> s)
makePrisms ''DeltaExt

newtype DeltaCon s x = DeltaCon (s -> Vector x -> s)
makePrisms ''DeltaCon

newtype Lambda s y = Lambda (s -> Vector y)
instance Functor (Lambda s) where
  fmap f (Lambda l) = Lambda $ fmap f . l
makePrisms ''Lambda

newtype Ta s = Ta (s -> T)
makePrisms ''Ta

data Model x s y =
  Model {
    _deltaInt :: DeltaInt s,
    _deltaExt :: DeltaExt s x,
    _deltaCon :: DeltaCon s x,
    _lambda   :: Lambda s y,
    _ta       :: Ta s
 }
makeClassy ''Model

instance Functor (Model x s) where
  fmap f m = Model {
       _deltaInt = m ^. deltaInt,
       _deltaExt = m ^. deltaExt,
       _deltaCon = m ^. deltaCon,
       _ta = m ^. ta,
       _lambda = fmap f $ m ^. lambda
    }
