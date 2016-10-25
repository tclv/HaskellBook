module WarmUp where

import Prelude hiding (lookup)
import Control.Applicative
import Data.Maybe
import Data.Monoid

x, y, z :: [Integer]
x = [1 .. 3]
y = [4 .. 6]
z = [7 .. 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup f = getFirst . foldMap (First . checkEq f)
  where
    checkEq a (x, y) = if a == x then Just y else Nothing

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

summed :: Num c => (c, c) -> c
summed = (uncurry (+))

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
--  print $ sequenceA [Just 3, Just 2, Just 1]
--  print $ sequenceA [x, y]
--  print $ sequenceA [xs, ys]
--  print $ summed <$> ((,) <$> xs <*> ys)
--  print $ fmap summed ((,) <$> xs <*> zs)
--  print $ bolt 7
--  print $ fmap bolt z
--
--  print $ sequenceA [(>3), (<8), even] 7
  print $ getAll . foldMap All $ sequA 22
  print $ sequA $ fromMaybe 2 s'
  print $ bolt $ fromMaybe 2 ys
