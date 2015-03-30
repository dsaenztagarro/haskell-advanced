module Main where

import Control.Arrow
import Control.Monad

count w = Kleisli readFile >>>
          arr words >>> arr (filter (==w)) >>> arr length >>>
          Kleisli print

-- Stream functions

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)
