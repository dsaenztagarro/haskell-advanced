module Main where

import Control.Monad

-- Count number of occurrences of a word in a string
count w = length . filter (==w) . words

-- Count number of occurrences of a word in a file
count' w = (>>=print) .
           liftM (length . filter (==w) . words) .
           readFile

-- Using Arrows (without using Control.Arrow)

type Kleisli m a b = a -> m b

(>>>) :: Monad m =>
         Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do b <- f a
                 g b

arr :: Monad m => (a -> b) -> Kleisli m a b
arr f = return . f

count'' w = readFile >>>
            arr words >>> arr (filter (==w)) >>> arr length >>>
            print

