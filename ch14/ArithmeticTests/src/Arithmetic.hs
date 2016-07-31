module Arithmetic where


import Data.Char (toUpper)


half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)


plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

powerCommutative :: Integral b => b -> b -> Bool
powerCommutative x y = x ^ y == y ^ x

reverseIdentity :: Eq a => [a] -> Bool
reverseIdentity x = (reverse . reverse) x == x

dollarSignLaw :: Eq b => (a -> b) -> a -> Bool
dollarSignLaw f a = ($) f a == f a

takeLength :: Int -> [a] -> Bool
takeLength n xs = length (take n xs) == n

square :: Double -> Double
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitalizeWord :: [Char] -> [Char]
capitalizeWord = map toUpper
