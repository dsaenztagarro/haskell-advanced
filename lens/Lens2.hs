{-# LANGUAGE RankNTypes #-}

import LensExample
import Control.Applicative (Const(..))
import Data.Functor.Identity

type Lens thing prop = Functor f => (prop -> f prop) -> thing -> f thing

view :: Lens thing prop -> thing -> prop
-- view ln thing = getConst $ ln (\p -> Const p) thing
view ln thing = getConst $ ln Const thing

update :: Lens thing prop -> (prop -> prop) -> thing -> thing
-- update ln fn thing = runIdentity $ ln (\p -> Identity (fn p)) thing
update ln fn thing = runIdentity $ ln (Identity . fn) thing

set :: Lens thing prop -> prop -> thing -> thing
-- set ln newValue thing = update ln (\_ -> newValue) thing
set ln newValue = update ln (const newValue)

betterAddressLens :: Lens Person Address
-- that function returns a new Address wrapped in some functor (we dont know which, remember).
-- We need to turn our functor wrapped Address into a functor wrapped Person then, remembering
-- to update the Person's Address to the new address in the process:
betterAddressLens fn person = fmap (\newAddy -> person { address = newAddy }) (fn $ address person)

betterNameLens :: Lens Person String
betterNameLens fn person = fmap (\newName -> person { name = newName }) (fn $ name person)

