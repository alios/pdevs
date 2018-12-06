{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Pdevs.Component
    ( ComponentPath, _ComponentPath, HasComponentPath(..)
    , HasComponentPaths(..), Component(..)
    , Z(..)
    , RootCoordinator(..)
    ) where

import           Control.Pdevs.AtomicModel
import           Data.Typeable (Typeable)
import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Cons
import           Control.Lens.Fold
import           Control.Lens.Prism
import           Control.Lens.TH
import           Data.Vector (Vector)

newtype ComponentPath  =
  ComponentPath (Vector Int)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

makePrisms ''ComponentPath
makeClassy ''ComponentPath

instance Cons ComponentPath ComponentPath Int Int where
  _Cons = prism' f t
    where
      f (i, (ComponentPath p)) = ComponentPath (i <| p)
      t (ComponentPath p) = do
        (i, p') <- preview _Cons p
        return (i, ComponentPath p')


class HasComponentPaths a where
  componentPaths :: (Monoid (f ComponentPath), Applicative f) =>
    Getter a (f ComponentPath)

data Z t d x y where
  BindInput  :: Component t d x' y' -> (x -> x')  -> Z t d x y
  BindOutput :: Component t d x' y' -> (y' -> y) -> Z t d x y
  BindIntern :: Component t d x' y' -> Component t d x'' y'' -> (y' -> x'') -> Z t d x y
  deriving (Typeable)

instance Show (Z t d x y) where
  show (BindInput c _) = mconcat
    ["BindInput (", show c, ")"]
  show (BindOutput c _) = mconcat
    ["BindOutput (", show c, ")"]
  show (BindIntern a b _) = mconcat
    ["BindIntern (", show a, ") (", show b, ")" ]

instance HasComponentPaths (Z t d x y) where
  componentPaths = to zComponentPaths
    where
      zComponentPaths (BindInput c _) = view componentPaths c
      zComponentPaths (BindOutput c _) = view componentPaths c
      zComponentPaths (BindIntern a b _) =
        view componentPaths a `mappend` view componentPaths b

data Component t (d :: [*]) x y where
  Simulator   :: ComponentPath -> AtomicModel t s x y -> s -> Component t d x y
  Coordinator :: ComponentPath -> Vector (Z t (d':d) x y) -> Component t d x y

instance HasComponentPath (Component t d x y) where
  componentPath = lens getter setter
    where getter (Simulator p _ _ ) = p
          getter (Coordinator p _) = p
          setter (Simulator _ m s ) p = Simulator p m s
          setter (Coordinator _ zs) p = Coordinator p zs

instance Show (Component t d x y) where
  show (Simulator p _ _) = "Simulator " <> show p
  show (Coordinator p zs) = mconcat ["Coordinator ", show p, " ", show zs]

instance HasComponentPaths (Component t d x y) where
  componentPaths = to componentPaths'
    where
      componentPaths' (Simulator p _ _) = pure p
      componentPaths' (Coordinator p zs) =
        foldl (\b -> mappend b . view componentPaths) (pure p) zs

newtype RootCoordinator t x y =
  RootCoordinator (Vector (Z t '[] x y))

instance Show (RootCoordinator t x y) where
  show (RootCoordinator zs) = "RootCoordinator " <> show zs

instance HasComponentPaths (RootCoordinator t x y) where
  componentPaths = to componentPaths'
    where
      componentPaths' (RootCoordinator zs) =
        foldl (\b -> mappend b . view componentPaths) mempty zs
