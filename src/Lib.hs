module Lib where

import           Control.Pdevs.AtomicModel
import           Control.Pdevs.Coupled

m1 :: AtomicModel Float () Int Double
m1 = defaultAtomicModel

t1 :: Monad m => m (RootCoordinator Float Int String)
t1 = mkRootCoordinator . mkCoordinator $ do
  s0 <- mkSimulator m1 ()
  c1 <- mkCoordinator $ pure ()

  c0 <- mkCoordinator $ do
    s1 <- mkSimulator m1 ()
    bindInput s1 id
    bindOutput s1 show
  bindIntern s0 c0 round
  bindInput s0 id
  bindOutput c0 id

printT1 :: IO ()
printT1 = t1 >>= rk
