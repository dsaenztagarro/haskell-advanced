module Main where

import Control.Arrow
import Control.Monad

count w = Kleisli readFile >>>
          arr words >>> arr (filter (==w)) >>> arr length >>>
          Kleisli print
