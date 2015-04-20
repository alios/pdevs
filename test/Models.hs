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


data PingPongPlayer

data PingPongSendMsg = PingPongSendMsg deriving (Typeable, Generic)
data PingPongRecvMsg = PingPongRecvMsg deriving (Typeable, Generic)
instance Binary PingPongSendMsg
instance Binary PingPongRecvMsg

instance Tish (Time Double) where
  toT i = i *~ second

instance Model PingPongPlayer where
  type X PingPongPlayer = PingPongRecvMsg
  type Y PingPongPlayer = PingPongSendMsg

instance AtomicModel PingPongPlayer where
  type T PingPongPlayer = Time Double
  data S PingPongPlayer =
    PingPongSend (T PingPongPlayer) |
    PingPongWait (T PingPongPlayer) deriving (Typeable, Generic)

  ta (PingPongSend t) = t
  ta (PingPongWait t) = t
  deltaInt (PingPongSend t) = PingPongWait $ t_infinity
  deltaInt (PingPongWait t) = PingPongSend $ 0.1 *~ second
  lambda (PingPongSend t) = Just PingPongSendMsg
  lambda (PingPongWait t) = Nothing
  deltaExt (PingPongWait _, _) _ = PingPongSend $ 0.1 *~ second
  deltaExt (s, _) _ = s
  deltaCon s _ = deltaInt s

instance Binary (S PingPongPlayer)

data PingPong

instance Model PingPong where
  type X PingPong = ()
  type Y PingPong = ()

instance CoupledModel PingPong where
  data CoupledModelRef PingPong = PingPong

  cm = do
    a <- newAtomicInstance . PingPongSend $ 0.1 *~ second
    b <- newAtomicInstance . PingPongWait $ ((P./) 1 0) *~ second
    bind a (\PingPongSendMsg -> PingPongRecvMsg) b


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

instance (Fractional t, Binary t) => Binary (Time t) where
  put = unitPut second
  get = unitGet second

unitPut :: (Fractional a, Binary a) => Unit d a -> Quantity d a -> Put
unitPut u i = put $ i /~ u

unitGet :: (Fractional a, Binary a) => Unit d a -> Get (Quantity d a)
unitGet u = fmap (\i -> i *~ u) get

