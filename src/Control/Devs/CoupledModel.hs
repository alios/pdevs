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

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Safe       #-}

-- | construct coupled models
module Control.Devs.CoupledModel
  ( -- * ModelReference
    ModelRef(..),
    -- * CoupledModel
    CoupledModel(..),
    -- ** defition of a CoupledModel
    CoupledModelMonad,
    newInstance, bindInput, bindOutput, bind,
    -- ** evaluate a CoupledModel
    CoupledAction(..),
    evalCoupledModel
  ) where

import           Control.Devs.CoupledModel.Types
import           Control.Monad.RWS

-- | create a 'ModelInstance' of the given 'ModelRef'.
newInstance :: ModelRef mt t tx ty ->
               CoupledModelMonad x y m (ModelInstance x y mt t tx ty)
newInstance r = do
  ic <- nextInstanceCount
  let i = Instance ic r
  tell [CoupledActionInstance i]
  return i

-- | bind the 'CoupledModel's input to a given 'ModelInstance'.
--   use the supplied function to map from 'CoupledModel's input 'CX'
--   to the input of the given 'ModelInstance'.
bindInput ::
  ModelInstance x y ma a ax ay -> (x -> ax) -> CoupledModelMonad x y m ()
bindInput i z = do
  bc <- nextBindingCount
  tell [CoupledActionBinding $ InputBinding bc i z]

-- | bind a 'ModelInstance' output to 'CoupledModel's output.
--   use the supplied function to map from 'ModelInstance's output
--   to the output of the 'CoupledModel'.
bindOutput ::
  ModelInstance x y ma a ax ay -> (x -> ax) -> CoupledModelMonad x y m ()
bindOutput i z = do
  bc <- nextBindingCount
  tell [CoupledActionBinding $ InputBinding bc i z]

-- | bind two 'ModelInstance's inside the 'CoupledModel'.
--   use the supplied function to map from 'a' output to 'b' input.
bind ::
  ModelInstance x y ma a ax ay ->
  (ay -> bx) ->
  ModelInstance x y mb b bx by ->
  CoupledModelMonad x y m ()
bind a z b = do
  bc <- nextBindingCount
  tell [CoupledActionBinding $ InternalBinding bc a z b]

--
-- helpers
--
nextInstanceCount :: CoupledModelMonad x y m Int
nextInstanceCount = state updateInstanceCount
  where updateInstanceCount s =
          let i = _instanceCount s
          in (i, s { _instanceCount = succ i })

nextBindingCount :: CoupledModelMonad x y m Int
nextBindingCount = state updateBindingCount
  where updateBindingCount s =
          let i = _bindingCount s
          in (i, s { _bindingCount = succ i })
