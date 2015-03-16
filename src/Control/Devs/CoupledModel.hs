{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Trustworthy           #-}

module Control.Devs.CoupledModel where

import           Control.Devs.CoupledModel.Types
import           Control.Devs.Model
import           Control.Lens
import           Control.Monad.Writer


modelInstance :: (HasModel t tx s ty) => String -> t -> s ->
                 CoupledModelM x y (ModelRef tx ty)
modelInstance n m s0 = do
  let ref = AtomicModelRef n (m ^. model) s0
  tell (return $ Instance ref)
  return ref

coupledInstance :: String -> CoupledModel x y ->
                   CoupledModelM x y (ModelRef tx ty)
coupledInstance n m = do
  let ref = CoupledModelRef n m
  tell (return $ Instance ref)
  return ref


bindInput  :: ModelRef tx ty -> (x -> tx) ->   CoupledModel x y
bindInput r = tellBinding . ZInput r

bindOutput :: ModelRef tx ty -> (ty -> y) ->   CoupledModel x y
bindOutput r = tellBinding . ZOutput r

influences :: ModelRef ax ay -> (ay -> bx) ->  ModelRef bx by -> CoupledModel x y
influences a z = tellBinding . ZInternal a z


tellBinding :: Z x y i j -> CoupledModel x y
tellBinding a = tell . return $ Binding a


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
