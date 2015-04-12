{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Models where

import           Control.Devs
import           Data.Binary
import qualified Data.Set                             as Set
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import qualified Numeric.NumType.TF                   as Num
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P




data A

instance Model A where
  type X A = MassFlow Double
  type Y A = Mass Double

instance AtomicModel A where
  type T A = Time Double
  toT i = i *~ second

  data S A = A deriving (Generic, Typeable)

instance Binary (S A)



data B

instance Model B where
  type X B = Mass Double
  type Y B = Length Double

instance AtomicModel B where
  type T B = Time Double
  toT i = i *~ second

  data S B = B deriving (Generic, Typeable)

instance Binary (S B)


data C

instance Model C where
  type X C = Length Double
  type Y C = Length Double

instance AtomicModel C where
  type T C = Time Double
  toT i = i *~ second

  data S C = C deriving (Generic, Typeable)

instance Binary (S C)


data CA

instance Model CA where
  type X CA = Mass Double
  type Y CA = Length Double

instance CoupledModel CA where

  data CoupledModelRef CA = CA
  cm = do
    b <- newAtomicInstance B
    c <- newAtomicInstance C
    bindInput b id
    bindOutput c id
    bind b id c


data CB

instance Model CB where
  type X CB = MassFlow Double
  type Y CB = Length Double

instance CoupledModel CB where

  data CoupledModelRef CB = CB
  cm = do
    a <- newAtomicInstance A
    ca <- newCoupledInstance CA
    bindInput a id
    bind a id ca
    b <- newAtomicInstance B
    bind a id b
    bindOutput ca id





{-

data Tank


data TankOutput =
  TankOutput { _tankEmpty :: Bool
             , _tankFull  :: Bool
             , _tankMass  :: Mass Double
             } deriving (Typeable, Generic)
instance Binary TankOutput

instance Model Tank where
  type X Tank = MassFlow Double
  type Y Tank = TankOutput

instance AtomicModel Tank where
  type T Tank = Time Double
  toT i = i *~ second

  data S Tank = Tank {
    _tankEmptyMass :: Mass Double,
    _tankSize :: Mass Double,
    _tankFill :: Mass Double,
    _tankRate :: MassFlow Double
    } deriving (Typeable, Generic)


  deltaInt st = st { _tankRate = 0 *~ (gram / second)
                   , _tankFill =
                     if (_tankFill st < 0 *~ gram) then (0 *~ gram)
                     else if (_tankFill st > _tankSize st) then _tankSize st
                          else _tankFill st
                   }

  deltaExt (st, t) xs =
    st { _tankFill = _tankFill st + (t * _tankRate st)
       , _tankRate = sum $ Set.toList xs }

  deltaCon st _ = deltaInt st

  lambda st = TankOutput {
    _tankMass = _tankEmptyMass st + _tankFill st,
    _tankFull = _tankFill st == _tankSize st,
    _tankEmpty = _tankFill st == 0 *~ gram
    }


  ta st =
    if (_tankRate st >= 0 *~ (gram / second))
    then (_tankSize st - _tankFill st)  / _tankRate st
    else (negate $ _tankFill st) / _tankRate st


instance Binary (S Tank)

type DHeatOfCombustion = Div DEnergy DMass
type HeatOfCombustion = Quantity DHeatOfCombustion

class Engines fuel where
  data Engine fuel :: *
  engEfficency :: Engine fuel -> Dimensionless Double
  fuelCapacity :: Engine fuel -> HeatOfCombustion Double
  fuelDensity :: Engine fuel -> Density Double

instance Model (Engine eng) where
  type X (Engine eng) = Double
  type Y (Engine eng) = (Energy Double, Double) -- Kraft, Verbrauch

instance (Typeable eng) => AtomicModel (Engine eng) where
  data S (Engine eng) = Engine deriving (Typeable, Generic)


instance Binary (S (Engine eng))

data Diesel

instance Engines Diesel where
  data Engine Diesel = DieselEngine {
    _dieselEngMaxPower :: Double
    } deriving (Typeable, Generic)

  engEfficency _ = 0.43 *~ one
  fuelCapacity _ = 45.4 *~ (mega joule / kilo gram)
  fuelDensity _ = 0.82 *~ (kilo gram / liter)

data OttoFuel

instance Engines OttoFuel where
  data Engine OttoFuel = OttoEngine {
    _ottoEngMaxPower :: Double
    } deriving (Typeable, Generic)

  engEfficency _ = 0.34 *~ one
  fuelCapacity _ = 42.7 *~ (mega joule / kilo gram)
  fuelDensity _ = 0.72 *~ (kilo gram / liter)

-}

instance (Fractional t, Binary t) => Binary (Mass t) where
  put = unitPut (kilo gram)
  get = unitGet (kilo gram)

instance (Fractional t, Binary t) => Binary (MassFlow t) where
  put = unitPut (kilo gram / second)
  get = unitGet (kilo gram / second)

instance (Fractional t, Binary t) => Binary (Length t) where
  put = unitPut meter
  get = unitGet meter

instance (Fractional t, Binary t) => Binary (Energy t) where
  put = unitPut (newton * meter)
  get = unitGet (newton * meter)

unitPut :: (Fractional a, Binary a) => Unit d a -> Quantity d a -> Put
unitPut u i = put $ i /~ u

unitGet :: (Fractional a, Binary a) => Unit d a -> Get (Quantity d a)
unitGet u = fmap (\i -> i *~ u) get

