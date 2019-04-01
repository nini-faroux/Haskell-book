module Chap11.Vehicles where

data Vehicle = 
    Car Manufacturer Price 
  | Plane Airline Size
  deriving (Eq, Show)

data Airline = 
    PapuAir
  | Catapults 
  | TUnited 
  deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
type Size = Integer

-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True 
isCar _ = False

isPlane :: Vehicle -> Bool 
isPlane (Plane _ _) = True 
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

-- 3
getManu :: Vehicle -> Maybe Manufacturer 
getManu (Car man _) = Just man
getManu _ = Nothing

--- 
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir
