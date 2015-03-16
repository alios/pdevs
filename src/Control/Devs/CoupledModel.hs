{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}

module Control.Devs.CoupledModel where

import           Control.Devs.Model
import           Control.Lens
import           Control.Monad.Writer
import           Data.Vector          (Vector)

data CoupledModelDef x y where
  Binding  :: Z x y i j      -> CoupledModelDef x y
  Instance :: ModelRef tx ty -> CoupledModelDef x y

type CoupledModelM x y a = MonadWriter (CoupledModelDef x y) m => m a
type CoupledModel x y = CoupledModelM x y ()

data ModelRef x y where
  AtomicModelRef :: String -> Model x s y -> s -> ModelRef x y
  CoupledModelRef :: String -> CoupledModel tx ty -> ModelRef x y

data Z x y i j where
  ZInput    :: (x -> tx) ->  ModelRef tx ty -> Z x y x tx
  ZInternal :: (ay -> bx) -> ModelRef ax ay -> ModelRef bx by -> Z x y ay bx
  ZOutput   :: (ty -> y) ->  ModelRef tx ty -> Z x y ty y


data ComponentInfluencer x y tx where
  ComponentInfluencer :: Either (Z x y x tx) (Z x y a tx) ->
                         ComponentInfluencer x y tx

data SelfInfluencer x y where
  SelfInfluencer :: Z x y ty y -> SelfInfluencer x y

data Component x y where
  Component :: ModelRef tx ty ->
               Vector (ComponentInfluencer x y tx) ->
               Component x y

newtype CoupledModelSpec x y =
  CoupledModelSpec (String, Vector (Component x y), Vector (SelfInfluencer x y))
makePrisms ''CoupledModelSpec


modelInstance :: (HasModel t tx s ty) => String -> t -> s ->
                 CoupledModelM x y (ModelRef tx ty)
modelInstance n m s0 = do
  let ref = AtomicModelRef n (m ^. model) s0
  tell . mappend mempty . Instance $ ref
  return ref

coupledInstance :: String -> CoupledModel x y ->
                   CoupledModelM x y (ModelRef tx ty)
coupledInstance n m = do
  let ref = CoupledModelRef n m
  tell . mappend mempty . Instance $ ref
  return ref


bindInput :: (x -> tx) ->  ModelRef tx ty -> CoupledModel x y
bindInput z ref  = tell . mappend mempty . Binding $ ZInput z ref

bindOutput :: (ty -> y) ->  ModelRef tx ty -> CoupledModel x y
bindOutput z ref = tell . mappend mempty . Binding $ ZOutput z ref

influences :: (ay -> bx) -> ModelRef ax ay -> ModelRef bx by -> CoupledModel x y
influences z a b = tell . mappend mempty . Binding $ ZInternal z a b

{-
  coupledModel :: CoupledModel x y ()
  coupledModel = do
    a <- modelInstance "A" modelA
    bindInput a
    b <- coupledModel "B" cmodelB
    a `influences` b
    c <- modelInstance "C" modelC
    b `influences` c
    bindOutput c
-}
