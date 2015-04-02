module Main where

import Control.Monad

count w = Kleisli readFile >>>
          arr words >>> arr (filter (==w)) >>> arr length >>>
          Kleisli print

-- Overloading arrow operators making Kleisli arrows an instance

class Arrow arr where
  arr :: (a -> b) -> arr a b
  (>>>) :: arr a b -> arr b c -> arr a c
  (&&&) :: arr a b -> arr a c -> arr a (b, c)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)
  (f &&& g) a = (f a, g a)

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  Kleisli f >>> Kleisli g = Kleisli (f >=> g)
  Kleisli f &&& Kleisli g = Kleisli $ \a -> do b <- f a
                                               c <- g a
                                               return (b, c)

-- Arrow of stream functions

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
  arr f = SF (map f)
  SF f >>> SF g = SF (f >>> g)
  SF f &&& SF g = SF (f &&& g >>> uncurry zip)

delay x = SF (x:)

pairPred = arr id &&& delay 0
