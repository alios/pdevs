{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE Trustworthy            #-}

module Control.Devs.Model
     ( T, DeltaInt, _DeltaInt, DeltaExt, _DeltaExt, DeltaCon, _DeltaCon
     , Lambda, _Lambda, Ta, _Ta, Model(..), HasModel(..)
     ) where


import           Control.DeepSeq
import           Control.Lens
import           Data.Typeable   (Typeable)
import           Data.Vector     (Vector)
import           GHC.Generics

type T = Double

newtype DeltaInt s = DeltaInt (s -> s) deriving (Typeable, Generic)
instance NFData (DeltaInt s)
makePrisms ''DeltaInt

newtype DeltaExt s x = DeltaExt ((s, T) -> Vector x -> s)
          deriving (Typeable, Generic)
instance NFData (DeltaExt s x)
makePrisms ''DeltaExt

newtype DeltaCon s x = DeltaCon (s -> Vector x -> s) deriving (Typeable, Generic)
instance NFData (DeltaCon s x)
makePrisms ''DeltaCon


newtype Lambda s y = Lambda (s -> Vector y) deriving (Typeable, Generic)

instance Functor (Lambda s) where
  fmap f (Lambda l) = Lambda $ fmap f . l

instance Profunctor Lambda where
  dimap fs fy (Lambda l) = Lambda $ fmap fy . l . fs

instance NFData (Lambda s y)
makePrisms ''Lambda



newtype Ta s = Ta (s -> T) deriving (Typeable, Generic)
instance NFData (Ta s)
makePrisms ''Ta

data Model x s y =
  Model {
    _deltaInt :: DeltaInt s,
    _deltaExt :: DeltaExt s x,
    _deltaCon :: DeltaCon s x,
    _lambda   :: Lambda s y,
    _ta       :: Ta s
 } deriving (Typeable, Generic)
instance NFData (Model x s y)

makeClassy ''Model

instance Functor (Model x s) where
  fmap f m = Model {
       _deltaInt = m ^. deltaInt,
       _deltaExt = m ^. deltaExt,
       _deltaCon = m ^. deltaCon,
       _ta = m ^. ta,
       _lambda = fmap f $ m ^. lambda
    }
