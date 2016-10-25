import Data.Char

import Control.Applicative

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = liftA2 (,) id composed

tupledM :: String -> (String, String)
tupledM = do
  a <- id
  b <- composed
  return (a, b)

tupledM' :: String -> (String, String)
tupledM' = id >>= (\x -> composed >>= (\y -> return $ (x, y)))

newtype Reader r a =
  Reader { getReader :: r -> a }

asksid :: Reader a a
asksid = Reader id

liftA2' :: Applicative f => (a -> b -> c) -> (f a) -> (f b) -> (f c)
liftA2' x y z = x <$> y <*> z

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rab <*> Reader ra = Reader $ \x -> rab x (ra x)

instance Monad (Reader r) where
  Reader ra >>= f = Reader $ \x -> getReader (f (ra x)) x

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName 
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address 
               } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") 
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu") 
               (Address "Austin")

getDogRm :: Reader Person Dog
getDogRm = do
  dog' <- Reader dogName
  add <- Reader address
  return $ Dog dog' add


