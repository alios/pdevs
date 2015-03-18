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
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Devs.CoupledModel.Types where

import           Control.DeepSeq
import           Control.Devs.AtomicModel
import           Control.Lens
import           Control.Monad.Writer
import           Data.Table
import           Data.Typeable            (Typeable, cast)
import           Data.Vector              (Vector)


data CoupledModelDef x y where
  Binding  :: Z x y i j      -> CoupledModelDef x y
  Instance :: ModelRef tx ty -> CoupledModelDef x y
  deriving (Typeable)

type CoupledModelM x y a = MonadWriter (Vector (CoupledModelDef x y)) m => m a
type CoupledModel x y = CoupledModelM x y ()

data ModelRef x y where
  AtomicModelRef :: (AtomicModel t) => String  -> S t -> ModelRef (X t) (Y t)
  CoupledModelRef :: String -> CoupledModel tx ty -> ModelRef tx ty
  deriving (Typeable)

{-
_ModelRefAtomic ::
                   Prism' (ModelRef x y) (String, (Model x s y, s))
_ModelRefAtomic = prism c b
  where c t = AtomicModelRef (fst t) (fst . snd $ t) (snd . snd $ t)
        b r@(CoupledModelRef _ _) = Left r
        b r@(AtomicModelRef n m s0) =
          case (cast (n,(m,s0))) of
           Nothing -> Left r
           Just ret -> Right ret
-}
{-_ModelRefAtomic = prism g
   where g = undefined
         s = undefined
-}

-- prism :: ((String, (Model tx ts ty)) -> (ModelRef x y)) ->
--          ((ModelRef x y) -> Either (ModelRef x y) (String, (Model x s y))) ->
--          Prism' (ModelRef x y) (String, (Model x s y))

data Z x y i j where
  ZInput    :: ModelRef tx ty -> (x -> tx) -> Z x y x tx
  ZInternal :: ModelRef ax ay -> (ay -> bx) -> ModelRef bx by -> Z x y ay bx
  ZOutput   :: ModelRef tx ty -> (ty -> y) -> Z x y ty y
  deriving (Typeable)

data ComponentInfluencer x y tx where
  ComponentInfluencer :: Either (Z x y x tx) (Z x y a tx) ->
                         ComponentInfluencer x y tx
  deriving (Typeable)
instance NFData (ComponentInfluencer x y tx)

data SelfInfluencer x y where
  SelfInfluencer :: Z x y ty y -> SelfInfluencer x y
  deriving (Typeable)

data Component x y tx ty = Component {
  _compModelRef    :: ModelRef tx ty,
  _compInfluencers :: Vector (ComponentInfluencer x y tx)
} deriving (Typeable)

makeLenses ''Component


data CoupledModelSpec x y =
  CoupledModelSpec {
    _coupledName     :: String,
    _coupledComps    :: Vector (forall tx ty. Component x y tx ty),
    _coupledSelfInfs :: Vector (SelfInfluencer x y)
    } deriving (Typeable)



makeLenses ''CoupledModelSpec

instance NFData (CoupledModelSpec x y)
instance NFData (Component x y tx ty)
instance NFData (SelfInfluencer x y)
instance NFData (Z x y i j)
instance NFData (ModelRef x y)
instance NFData (CoupledModelDef x y)

instance Show (CoupledModelDef x y) where
  show (Binding z) = "Binding " ++ show z
  show (Instance r) = "Instance " ++ show r

modelRefName :: Lens' (ModelRef x y) String
modelRefName = lens g s
  where g (AtomicModelRef n _) = n
        g (CoupledModelRef n _) = n
        s (AtomicModelRef _ s0) n = AtomicModelRef n s0
        s (CoupledModelRef _ m) n = CoupledModelRef n m


instance Tabular (ModelRef x y) where
  type PKT (ModelRef x y) = Int
  data Key k (ModelRef x y) b where
    ModelRefId :: Key Primary (ModelRef x y) Int
    ModelRefName :: Key Supplemental (ModelRef x y) String
  data Tab (ModelRef x y) i =
    ModelRefTab (i Primary Int) (i Supplemental String)

  fetch ModelRefName = view modelRefName

instance Show (ModelRef x y) where
  show = view modelRefName

instance Show (Z x y i j) where
  show (ZInput r _) = "ZInput " ++ show r
  show (ZOutput r _) = "ZOutput " ++ show r
  show (ZInternal a _ b) = "ZInternal " ++ show a ++ " " ++ show b
