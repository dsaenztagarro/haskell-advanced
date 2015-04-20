module LensExample where

-- define our person and address data structures:
data Person = Person {
    name :: String,
    age :: Int,
    address :: Address
} deriving (Show)

data Address = Address {
    house :: Int,
    street :: String,
    city :: String
} deriving (Show)

-- make a new person:
james = Person {
    name = "James",
    age = 28,
    address = Address {
        house = 42,
        street = "Some Road",
        city = "London"
    }
}
