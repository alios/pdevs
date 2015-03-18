{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | construct coupled models
module Control.Devs.CoupledModel
  ( -- * types HasModel (..)
    AtomicModel (..), ModelRef,
    -- * instance models
    -- ** atomic models
    modelInstance,
    -- ** coupled models
    coupledInstance,
    -- * bind models
    bindInput, bindOutput, influences
  ) where

import           Control.Devs.CoupledModel.Types
import           Control.Devs.Model
import           Control.Lens
import           Control.Monad.Writer
import           Data.Typeable
import qualified Data.Vector                     as V


-- | make an instance of an atomic model implementing 'HasModel',
--   given a /name/, the model and a initial state.
--   a instanciated model will be called component and is reffered to by
--   a 'ModelRef'.
modelInstance :: (AtomicModel t) => String -> t -> S t ->
                 CoupledModelM x y (ModelRef (X t) (Y t))
modelInstance n m s0 = do
  let ref = AtomicModelRef n m s0
  tell (return $ Instance ref)
  return ref

-- | make an instance of another coupled model, given a /name/, the model and
--   an initial state.
--   a instanciated model will be called component and is reffered to by
--   a 'ModelRef'.
coupledInstance :: String -> CoupledModel tx ty ->
                   CoupledModelM x y (ModelRef tx ty)
coupledInstance n m = do
  let ref = CoupledModelRef n m
  tell (return $ Instance ref)
  return ref

-- | bind the input of the coupled model itself to a given component and
--   a mapping function from the coupled models input to the
--   components input.
bindInput  :: ModelRef tx ty -> (x -> tx) ->   CoupledModel x y
bindInput r = tellBinding . ZInput r

-- | bind the output of a component to the output of the coupled model itself,
--   given the component and a mapping function from the components
--   output to the output of the model itself
bindOutput :: ModelRef tx ty -> (ty -> y) ->   CoupledModel x y
bindOutput r = tellBinding . ZOutput r

-- | bind two components using a mapping function from component /a/ output
--   to component /b/ input.
influences :: ModelRef ax ay -> (ay -> bx) ->  ModelRef bx by -> CoupledModel x y
influences a z = tellBinding . ZInternal a z



tellBinding :: Z x y i j -> CoupledModel x y
tellBinding a = tell . return $ Binding a

--   CoupledModelSpec (String, Vector (Component x y), Vector (SelfInfluencer x y))

emptyCoupledModel :: String -> CoupledModelSpec x y
emptyCoupledModel n = CoupledModelSpec n mempty mempty

newComponent :: ModelRef tx ty -> Component x y tx ty
newComponent r =  Component r mempty

newSelfInfluencer :: Z x y ty y -> SelfInfluencer x y
newSelfInfluencer z = SelfInfluencer z




data A = A
instance AtomicModel A where
  type X A = String
  type Y A = Int
  data S A = StateA

data B = B
instance AtomicModel B where
  type X B = ()
  type Y B = Double
  data S B = StateB


{-
foo :: CoupledModel Int ()
foo = do
  a <- modelInstance "modelA" A ()
  bindInput a id
  b <- modelInstance "modelB" B (0 :: Double)
  influences a show b
  bindOutput b id

bar :: CoupledModel Int Char
bar = do
  a <- modelInstance "modelA" A ()
  b <- coupledInstance "foo" foo
  bindInput b id
  influences b (\_ -> 0) a
  bindOutput a id
-}

--f = snd $ runWriter foo
