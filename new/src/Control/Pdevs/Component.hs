{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Control.Pdevs.Component
    ( ComponentPath, Component(..), Z(..)
    ) where

import           Control.Pdevs.AtomicModel

type ComponentPath  = [Int]

data Z t d x y where
  BindInput  :: Component t d x' y' -> (x -> x')  -> Z t d x y
  BindOutput :: Component t d x' y' -> (y' -> y) -> Z t d x y
  BindIntern :: Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> Z t d x y

instance Show (Z t d x y) where
  show (BindInput c _) = mconcat
    ["BindInput (", show c, ")"]
  show (BindOutput c _) = mconcat
    ["BindOutput (", show c, ")"]
  show (BindIntern a b _) = mconcat
    ["BindIntern (", show a, ") (", show b, ")" ]

data Component t (d :: [*]) x y where
  Simulator   :: ComponentPath -> AtomicModel t s x y -> s -> Component t d x y
  Coordinator :: ComponentPath -> [Z t (d':d) x y] -> Component t d x y

instance Show (Component t d x y) where
  show (Simulator p _ _) = "Simulator " <> show p
  show (Coordinator p zs) = mconcat ["Coordinator ", show p, " ", show zs]
