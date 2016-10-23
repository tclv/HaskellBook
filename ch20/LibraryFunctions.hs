module LibraryFunctions where

import Prelude hiding (maximum, minimum, sum, product, null, length)

import Data.Monoid
import Data.Semigroup hiding (First, getFirst, (<>))


sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (x ==))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = fmap getMin . getOption . foldMap (Option . Just . Min)
  
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = fmap getMax . getOption . foldMap (Option . Just . Max)

null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (const (All False))

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (const (Sum 1))

toList :: (Foldable t) => t a -> [a]
-- toList  = foldMap (: [])
toList = foldr (:) []  -- probably better

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty
