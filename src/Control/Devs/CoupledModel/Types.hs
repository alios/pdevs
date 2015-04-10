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

{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE Safe         #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Devs.CoupledModel.Types where

import           Control.Devs.AtomicModel
import           Control.Monad.RWS
import           Data.IntMap              (IntMap)

type CM m a = CoupledModel m => RWS () [Binding m] (IntMap (Component m)) a

class (Model m) => CoupledModel m where
  data CoupledModelRef m :: *
  cm :: CM m ()

data Component m where
 MkComponent :: (CoupledModel m, Model a) => ModelInstance m a -> Component m

data Binding m where
  Bind :: (Model a, Model b, CoupledModel m) =>
    ModelInstance m a -> (Y a -> X b) -> ModelInstance m b -> Binding m
  BindInput :: (Model a, CoupledModel m) =>
    ModelInstance m a -> (X m -> X a) -> Binding m
  BindOutput :: (Model a, CoupledModel m) =>
    ModelInstance m a -> (Y a -> Y m) -> Binding m

data ModelInstance m a where
  AModel :: (CoupledModel m, AtomicModel a) =>
    Int -> S a -> ModelInstance m a
  CModel :: (CoupledModel m, CoupledModel a) =>
    Int -> CoupledModelRef a -> ModelInstance m a


