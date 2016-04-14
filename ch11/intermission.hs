{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)
-- 1 Type constructor
-- 2 * -> *
-- 3 *
-- 4 Num a => Doggies a
-- 5 Doggies Integer
-- 6 Doggies String
-- 7 both
-- 8 DogueDeBordeaux a
-- 9 DogueDeBordeaux String

data Price = Price Integer deriving (Eq, Show)

-- 5
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir 
             | CapaltusR'Us 
             | TakeYourCHanceUnited
               deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
               deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 200)

-- 1 myCar :: Vehicle
-- 2:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

-- 5 (Plane _) -> (Plane _ _)
isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3:
getManu :: Vehicle -> Manufacturer
getManu (Car a _) = a

-- 4 Partial function, because of the non-exhaustive pattern.
-- Will raise an exception if a Plane is feeded.
--
-- 5


-- 1 |PugType| = 1
-- 2 |Airline| = 3
-- 3 |Int16| -> [-32768, 32767] = 65536
-- 4 Cardinality of Int is big!, Integer has infinite cardinality as its unbounded
-- 5 2 ^ 8

data Example = MakeExample deriving Show

-- 1 MakeExample :: Example
-- 2 Yes we can see the instances defined in GHCi
-- 3
data Container a = MakeContainer a deriving (Show, Eq)
-- :t MakeContainer :: a -> Container a



class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cow = Cow (Int, String) deriving (Eq, Show)

data Herd = Herd (Int, String) deriving (Eq, Show)

instance TooMany Herd where
  tooMany (Herd (a, b)) = a > 42

newtype DoubleGoat = DoubleGoat (Int, Int) deriving (Eq, Show)

instance TooMany Cow where
  tooMany (Cow (a, b)) = a > 42

instance TooMany DoubleGoat where
  tooMany (DoubleGoat (a, b)) = (a * b) > 42

instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (a, b) = (a * b) > 42
  
-- 1 |BigSmall| = 2
-- 2 |NumberOrBool| = 2 + |Int8| = 258
