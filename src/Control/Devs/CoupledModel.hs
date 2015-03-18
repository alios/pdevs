{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-
Copyright (c) 2015, Markus Barenhoff <alios@alios.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}


-- | construct coupled models
module Control.Devs.CoupledModel
  ( -- * types
    ModelRef,
    -- * define model instances
    -- ** atomic models
    modelInstance,
    -- ** coupled models
    coupledInstance,
    -- * bind models
    bindInput, bindOutput, influences
  ) where

import           Control.Devs.AtomicModel
import           Control.Devs.CoupledModel.Types
import           Control.Monad.Writer
import           Data.Binary
import           Data.Typeable
import           GHC.Generics                    (Generic)

-- | make an instance of an atomic model implementing 'HasModel',
--   given a /name/, the model and a initial state.
--   a instanciated model will be called component and is reffered to by
--   a 'ModelRef'.
modelInstance :: (AtomicModel t) => String -> S t ->
                 CoupledModelM x y (ModelRef (X t) (Y t))
modelInstance n s0 = do
  let ref = AtomicModelRef n s0
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


newSelfInfluencer :: Z x y ty y -> SelfInfluencer x y
newSelfInfluencer z = SelfInfluencer z



data A deriving (Typeable, Generic)

instance AtomicModel A where
  type X A = String
  type Y A = Int
  data S A = StateA

deriving instance Generic (S A)
instance Binary (S A)

data B deriving (Typeable, Generic)
instance AtomicModel B where
  type X B = Int
  type Y B = Double
  data S B = StateB

deriving instance Generic (S B)
instance Binary (S B)

testC1 :: CoupledModel String Double
testC1 = do
  a <- modelInstance "A" StateA
  bindInput a id
  b <- modelInstance "B" StateB
  influences a id b
  bindOutput b id


testC2 :: CoupledModel String Int
testC2 = do
  a <- modelInstance "A" StateA
  b <- coupledInstance "testC1" testC1
  bindInput b id
  influences b show a
  bindOutput a id

c1 = snd $ runWriter testC1
c2 = snd $ runWriter testC2
