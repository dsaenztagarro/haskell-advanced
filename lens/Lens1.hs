import LensExample

-- get his house number by first getting the address from james,
-- and then getting the house number from address:
houseNumber = house (address james)

-- set his house number (creating a new person in the process):
updatedJames = james { address = (address james) { house = 43 } }

-- get the persons address (address person) and update house/street to a new value,
-- also updating person to use this updated address:
setHouse person value = person { address = (address person) { house = value }  }
setStreet person value = person { address = (address person) { street = value }  }

newPerson1 = setHouse james 45
newPerson2 = setStreet james "New Street"

-- The Lens way...

data Lens' thing prop = Lens' {
    view' :: thing -> prop,
    update' :: (prop -> prop) -> thing -> thing
}

set' :: Lens' thing prop -> prop -> thing -> thing
set' ln newValue thing = (update' ln) (\_ -> newValue) thing

addressLens :: Lens' Person Address
addressLens = Lens' address (\fn thing -> thing { address = fn (address thing) })

cityLens :: Lens' Address String
cityLens = Lens' city (\fn thing -> thing { city = fn (city thing) })

personToCityLens :: Lens' Person String
personToCityLens = Lens' ((view' cityLens).(view' addressLens)) ((update' addressLens).(update' cityLens))

--alternately, don't create a new lens and just compose the city and address lenses on the fly:
-- viewCity person = (view' cityLens . view' addressLens) person
-- updateCity fn person = (update' addressLens . update' cityLens) fn person

--this could be abbreviated as was done in the lens definition to:
viewCity = view' cityLens . view' addressLens
updateCity = update' addressLens . update' cityLens

-- > view' addressLens james
-- Address {house = 42, street = "Some Road", city = "London"}
-- > view' personToCityLens james
-- "London"
-- > set' personToCityLens "Paris" james
-- Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "Paris"}}
-- > update' personToCityLens (++"!!!!") james
-- Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London!!!!"}}
-- > (update' addressLens . update' cityLens) (++"!!!!") james
-- Person {name = "James", age = 28, address = Address {house = 42, street = "Some Road", city = "London!!!!"}}