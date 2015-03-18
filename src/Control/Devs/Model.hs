{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}

module Control.Devs.Model where





import           Data.Vector (Vector)


type T = Double


class AtomicModel m where
  type X m :: *
  type Y m :: *
  data S m :: *

  deltaInt :: S m -> S m
  deltaExt :: (S m, T) -> Vector (X m) -> S m
  deltaCon :: S m -> Vector (X m) -> S m
  lambda   :: S m -> Y m
  ta       :: S m -> T

