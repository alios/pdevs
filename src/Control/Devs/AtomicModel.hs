{-# LANGUAGE FlexibleContexts   #-}
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

{-# LANGUAGE Safe               #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The type class for /Atomic Models/.
module Control.Devs.AtomicModel
       ( module Control.Devs.Model
       , AtomicModel(..), Tish(..), t_infinity
       ) where

import           Control.Devs.Model
import           Data.Binary
import           Data.Set           (Set)
import           Data.Typeable


-- | types which can be used as 'T'
class Tish t where
  -- | convert a 'Double' to time.
  toT :: Double -> t

instance Tish Double where
  toT = id

instance Tish Float where
  toT = fromRational . toRational

instance Tish Integer where
  toT = round


-- | The /Parallel DEVS (P-DEVS)/ Model [PDEVS94].
class (Model m, Typeable m, Binary (S m), Tish (T m)) =>
      AtomicModel m where

  -- | the time type
  type T m :: *

  -- | the state type
  data S m :: *

  -- | the internal transition function
  deltaInt :: S m -> S m
  deltaInt = id

  -- | the external transition function
  deltaExt :: (S m, T m) -> Set (X m) -> S m
  deltaExt (st, _) _ = st

  -- | the confluent transition function
  deltaCon :: S m -> Set (X m) -> S m
  deltaCon st _ = st

  -- | the output function
  lambda   :: S m -> Maybe (Y m)

  -- | the time-advance function
  ta       :: S m -> T m

deriving instance Typeable S

-- | the infinite time interval
t_infinity :: (Tish t) => t
t_infinity = toT infinityT
infinityT :: (Fractional t) => t
infinityT = 1 / 0


-- $references
-- * [PDEVS94] Chow, A.C.; Zeigler, B.P., /Parallel DEVS: a parallel, hierarchical, modular modeling formalism/, Simulation Conference Proceedings, 1994. Winter, pp.716,722, 11-14 Dec. 1994, URL: <http://www.bgc-jena.mpg.de/~twutz/devsbridge/pub/chow96_parallelDEVS.pdf>
