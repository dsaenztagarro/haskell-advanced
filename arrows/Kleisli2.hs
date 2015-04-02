module Main where

import Control.Monad

count w = Kleisli readFile >>>
          arr words >>> arr (filter (==w)) >>> arr length >>>
          Kleisli print

-- Overloading arrow operators making Kleisli arrows an instance

class Arrow arr where
  arr :: (a -> b) -> arr a b
  (>>>) :: arr a b -> arr b c -> arr a c

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  Kleisli f >>> Kleisli g = Kleisli (f >=> g)

-- Arrow of stream functions

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
  arr f = SF (map f)
  SF f >>> SF g = SF (f >>> g)
