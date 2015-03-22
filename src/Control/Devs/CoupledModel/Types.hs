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


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Devs.CoupledModel.Types where


import           Control.Devs.AtomicModel
import           Control.Lens
import           Control.Monad.RWS
import           Data.Binary              (Binary)
import           Data.Typeable            (Typeable, cast)


--
-- CoupledModelDef / CoupledModelMoand
--
type CoupledModelDef x y = [Either (CoupledAction 'InstanceAction x y)
                            (CoupledAction 'BindAction x y)]

type CoupledModelMonad x y m a = (Monad m) =>
  RWST () (CoupledModelDef x y) CoupledModelBuilder m a


class ( Typeable t, Typeable (CX t), Typeable (CY t)
      , Binary (CX t), Binary (CY t)) =>
      CoupledModel t where
  type CX t :: *
  type CY t :: *

  coupledModelDef :: t -> CoupledModelMonad (CX t) (CY t) m ()

-- | evaluate a given 'CoupledModel'
evalCoupledModel :: (Monad m, CoupledModel t) =>
                    t -> m (CoupledModelDef (CX t) (CY t))
evalCoupledModel t = do
    let s = CoupledModelBuilder 0 0
    (_, as) <- execRWST (coupledModelDef t) () s
    return as


--
-- ModelRef
--
data ModelType = Atomic | Coupled | UndefModel deriving (Typeable)

deriving instance Typeable 'Atomic
deriving instance Typeable 'Coupled

data ModelRef mt t x y where
  AtomicModelRef  :: (AtomicModel t)  =>
                     S t -> ModelRef 'Atomic t (X t) (Y t)
  CoupledModelRef :: (CoupledModel t) =>
                     t   -> ModelRef 'Coupled t (CX t) (CY t)

deriving instance Typeable ModelRef

_AtomicModelRef :: AtomicModel t => Iso' (S t) (ModelRef 'Atomic t (X t) (Y t))
_AtomicModelRef = iso _f _t
  where _f = AtomicModelRef
        _t ((AtomicModelRef s0) :: ModelRef 'Atomic t (X t) (Y t)) = s0

_CoupledModelRef :: CoupledModel t => Iso' t (ModelRef 'Coupled t (CX t) (CY t))
_CoupledModelRef = iso _f _t
  where _f = CoupledModelRef
        _t ((CoupledModelRef t) :: ModelRef 'Coupled t (CX t) (CY t)) = t




--
-- Binding
--
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

deriving instance Typeable Binding


_InputBinding :: Prism' (Binding x y ma a ax ay 'UndefModel () () ())
                         (Int, ModelInstance x y ma a ax ay, x -> ax)
_InputBinding = prism' _f _t
  where _f (i, m, z) = InputBinding i m z
        _t ((InputBinding i m z) ::
               Binding x y ma a ax ay 'UndefModel () () ()) = Just (i, m, z)
        _t _ = Nothing


_OutputBinding :: Prism' (Binding x y ma a ax ay 'UndefModel () () ())
                         (Int, ModelInstance x y ma a ax ay, ay -> y)
_OutputBinding = prism' _f _t
  where _f (i, m, z) = OutputBinding i m z
        _t ((OutputBinding i m z) ::
               Binding x y ma a ax ay 'UndefModel () () ()) = Just (i, m, z)
        _t _ = Nothing

_InternalBinding :: Prism' (Binding x y ma a ax ay mb b bx by)
               (Int, ModelInstance x y ma a ax ay, (ay -> bx),
                ModelInstance x y mb b bx by)
_InternalBinding = prism' _f _t
  where _f (i,a,z,b) = InternalBinding i a z b
        _t ((InternalBinding i a z b) :: Binding x y ma a ax ay mb b bx by) =
          Just (i,a,z,b)
        _t _ = Nothing


--
-- Model Instance
--
data ModelInstance x y mt t tx ty where
  Instance :: Int -> ModelRef mt t tx ty -> ModelInstance x y mt t tx ty

deriving instance Typeable ModelInstance

_ModelInstance :: Iso' (Int, ModelRef mt t tx ty) (ModelInstance x y mt t tx ty)
_ModelInstance = iso (\(i,r) -> Instance i r) (\(Instance i r) -> (i,r))

modelInstanceRef' :: ModelInstance x y mt t tx ty -> ModelRef mt t tx ty
modelInstanceRef' a = a ^. re  _ModelInstance . _2

modelInstanceI :: ModelInstance x y mt t tx ty -> Int
modelInstanceI a = a ^. re _ModelInstance . _1


modelInstanceRef :: ( AtomicModel t, CoupledModel t2
                    , Typeable ma, Typeable ta, Typeable ax, Typeable ay) =>
                    (ModelInstance x y ma ta ax ay) ->
                    (Either
                     (ModelRef 'Atomic t (X t) (Y t))
                      (ModelRef 'Coupled t2 (CX t2) (CY t2) ))
modelInstanceRef a =
  let ca = case (cast $ modelInstanceRef' a) of
        Just (a' :: ModelRef 'Atomic t (X t) (Y t)) -> Just $ Left a'
        Nothing -> Nothing
      cb = case (cast $ modelInstanceRef' a) of
        Just (a' :: ModelRef 'Coupled t2 (CX t2) (CY t2)) -> Just $ Right a'
        Nothing -> Nothing
      cab = maybe (error "modelInstanceRef: got undefined instance ref") id cb
  in maybe cab id ca



--
-- CoupledAction
--
data CoupledActionT = InstanceAction | BindAction deriving (Typeable)

data CoupledAction (t :: CoupledActionT) x y where
  CoupledActionBinding ::
    Binding x y ma a ax ay mb b bx by -> CoupledAction 'BindAction x y
  CoupledActionInstance ::
    ModelInstance x y ma a ax ay -> CoupledAction 'InstanceAction x y

deriving instance Typeable CoupledAction


{-
foo :: (Typeable t, Typeable x, Typeable y, Typeable ma, Typeable a, Typeable ax, Typeable ay) =>
       CoupledAction (t :: CoupledActionT) x y ->
       Maybe (ModelInstance x y ma a ax ay)
-}

--foo (CoupledActionBinding b)  = Left b

{-
_CoupledActionBinding :: Prism' (CoupledAction 'BindAction x y)
                                 (Binding x y ma a ax ay mb b bx by)
_CoupledActionBinding = prism' CoupledActionBinding _f
-}




--
-- CoupledModelBuilder
--

data CoupledModelBuilder =
  CoupledModelBuilder { _instanceCount :: Int
                      , _bindingCount  :: Int
                      } deriving (Typeable)



