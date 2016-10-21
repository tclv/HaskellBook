module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure = flip Cons Nil
  fs <*> xs = flatMap (<$> xs) fs

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

main :: IO ()
main = do
  quickBatch $ functor $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil

  quickBatch $ functor $ ZipList' $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ ZipList' $ Cons ('a', 'b', 'c') Nil


take' :: Int -> List a -> List a
take' n xs
  | n <= 0    = Nil
  | otherwise = case (xs) of
                  Cons x xs -> Cons x $ take' (n - 1) xs
                  Nil       -> Nil

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = fmap ZipList' arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                 in take' 3000 l
          ys' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  ZipList' a <*> ZipList' b = ZipList' $ zipWith' ($) a b

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x
      
zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith' f xs ys
zipWith' _ _ _                     = Nil

zip' :: List a -> List b -> List (a, b)
zip' = zipWith' (,)
