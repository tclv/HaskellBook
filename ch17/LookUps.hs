module LookUps where

import Control.Applicative
import Data.List (elemIndex)

-- 1.

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.

x3 :: Maybe Int
x3 = elemIndex 3 [1 .. 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

-- 4.

xs, ys :: [Integer]
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x4 <*> y4
