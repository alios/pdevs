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


{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE Safe         #-}
{-# LANGUAGE TypeFamilies #-}


module Control.Devs.CoupledModel.Types where


import           Control.Devs.AtomicModel
import           Control.Monad.RWS


type CoupledModelMonad x y m a = (Monad m) =>
  RWST () [CoupledAction x y] CoupledModelBuilder m a


class CoupledModel t where
  type CX t :: *
  type CY t :: *

  coupledModelDef :: t -> CoupledModelMonad (CX t) (CY t) m ()

data ModelType = Atomic | Coupled | UndefModel

data ModelRef mt t x y where
  AtomicModelRef  :: (AtomicModel t)  =>
                     S t -> ModelRef 'Atomic t (X t) (Y t)
  CoupledModelRef :: (CoupledModel t) =>
                     t   -> ModelRef 'Coupled t (CX t) (CY t)

data Binding x y ma a ax ay mb b bx by where
  InputBinding ::
    Int -> ModelInstance x y ma a ax ay -> (x -> ax) ->
    Binding x y ma a ax ay 'UndefModel () () ()
  OutputBinding ::
    Int -> ModelInstance x y ma a ax ay -> (ay -> y) ->
    Binding x y ma a ax ay 'UndefModel () () ()
  InternalBinding ::
    Int -> ModelInstance x y ma a ax ay -> (ay -> bx) ->
    ModelInstance x y mb b bx by -> Binding x y ma a ax ay mb b bx by

data ModelInstance x y mt t tx ty where
  Instance :: Int -> ModelRef mt t tx ty -> ModelInstance x y mt t tx ty


data CoupledAction x y where
  CoupledActionBinding ::
    Binding x y ma a ax ay mb b bx by -> CoupledAction x y
  CoupledActionInstance ::
    ModelInstance x y ma a ax ay -> CoupledAction x y

data CoupledModelBuilder =
  CoupledModelBuilder { _instanceCount :: Int
                      , _bindingCount  :: Int
                      }


-- | evaluate a given 'CoupledModel'
evalCoupledModel :: (Monad m, CoupledModel t) =>
                    t -> m [CoupledAction (CX t) (CY t)]
evalCoupledModel t = do
    let s = CoupledModelBuilder 0 0
    (_, as) <- execRWST (coupledModelDef t) () s
    return as

