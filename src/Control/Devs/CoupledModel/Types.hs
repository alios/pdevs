{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}

module Control.Devs.CoupledModel.Types where

import           Control.DeepSeq
import           Control.Devs.Model
import           Control.Lens
import           Control.Monad.Writer
-- import           Data.Binary
import           Data.Typeable        (Typeable)
import           Data.Vector          (Vector)
import           GHC.Generics

data CoupledModelDef x y where
  Binding  :: Z x y i j      -> CoupledModelDef x y
  Instance :: ModelRef tx ty -> CoupledModelDef x y
  deriving (Typeable)
instance NFData (CoupledModelDef x y)

type CoupledModelM x y a = MonadWriter (Vector (CoupledModelDef x y)) m => m a
type CoupledModel x y = CoupledModelM x y ()

data ModelRef x y where
  AtomicModelRef :: String -> Model x s y -> s -> ModelRef x y
  CoupledModelRef :: String -> CoupledModel tx ty -> ModelRef tx ty
  deriving (Typeable)
instance NFData (ModelRef x y)

data Z x y i j where
  ZInput    :: ModelRef tx ty -> (x -> tx) -> Z x y x tx
  ZInternal :: ModelRef ax ay -> (ay -> bx) -> ModelRef bx by -> Z x y ay bx
  ZOutput   :: ModelRef tx ty -> (ty -> y) -> Z x y ty y
  deriving (Typeable)
instance NFData (Z x y i j)


data ComponentInfluencer x y tx where
  ComponentInfluencer :: Either (Z x y x tx) (Z x y a tx) ->
                         ComponentInfluencer x y tx
  deriving (Typeable)
instance NFData (ComponentInfluencer x y tx)


data SelfInfluencer x y where
  SelfInfluencer :: Z x y ty y -> SelfInfluencer x y
  deriving (Typeable)
instance NFData (SelfInfluencer x y)

data Component x y where
  Component :: ModelRef tx ty ->
               Vector (ComponentInfluencer x y tx) ->
               Component x y
  deriving (Typeable)
instance NFData (Component x y)

newtype CoupledModelSpec x y =
  CoupledModelSpec (String, Vector (Component x y), Vector (SelfInfluencer x y))
  deriving (Typeable, Generic)
instance NFData (CoupledModelSpec x y)

--instance Binary (CoupledModelSpec x y)

makePrisms ''CoupledModelSpec

