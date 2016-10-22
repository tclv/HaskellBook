module ChapterExercises where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
 - 1.
 -
 -pure  :: a -> [a]
 -(<*>) :: [(a -> b)] -> [a] -> [b]
 -
 - 2.
 -
 -pure  :: a -> IO a
 -(<*>) :: IO (a -> b) -> IO a -> IO b
 -
 - 3.
 -
 -pure  :: Monoid a => b -> (a, b)
 -(<*>) :: Monoid c => (c, a -> b) -> (c, a) -> (c, b)
 -
 - 4.
 -
 -pure  :: a -> (->) e a == a -> (e -> a) == a -> e -> a
 -(<*>) :: ((->) e (a -> b)) -> ((->) e a) -> ((->) e b)
 -(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
 -f (<*>) g = \x -> f x $ g x
 -}


main = do
  quickBatch $ functor $ (Pair (1, 2, 3) (1, 2, 3) :: Pair (Int, Int, Int))
  quickBatch $ applicative $ (Pair (1, 2, 3) (1, 2, 3) :: Pair (Int, Int, Int))

  quickBatch $ functor $ (Two "a" (1, 2, 3) :: Two String (Int, Int, Int))
  quickBatch $ applicative $ (Two "a" (1, 2, 3) :: Two String (Int, Int, Int))

  quickBatch $ functor $ (Three "a" 1 (1, 2, 3) :: Three String (Sum Int) (Int, Int, Int))
  quickBatch $ applicative $ (Three "a" 2 (1, 2, 3) :: Three String (Sum Int) (Int, Int, Int))

  quickBatch $ functor $ (Three' "a" (1, 2, 3) (1, 2, 3) :: Three' String (Int, Int, Int))
  quickBatch $ applicative $ (Three' "a" (1, 2, 3) (1, 2, 3) :: Three' String (Int, Int, Int))

  quickBatch $ functor $ (Four "a" "a" 1 (1, 2, 3) :: Four String String (Sum Int) (Int, Int, Int))
  quickBatch $ applicative $ (Four "a" "a" 2 (1, 2, 3) :: Four String String (Sum Int) (Int, Int, Int))

  quickBatch $ functor $ (Four' "a" "a" (1, 2, 3) (1, 2, 3) :: Four' String (Int, Int, Int))
  quickBatch $ applicative $ (Four' "a" "a"(1, 2, 3) (1, 2, 3) :: Four' String (Int, Int, Int))
  
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance Monoid a => Applicative (Two a) where
  pure = Two mempty 
  Two a f <*> Two a' x = Two (a `mappend` a') $ f x

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b $ f x

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a `mappend` a') (b `mappend` b') $ f x

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x x') = Three' a (f x) (f x')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Monoid a) => Applicative (Three' a)  where
  pure x = Three' mempty x x
  Three' a f g <*> Three' a' x y = Three' (a `mappend` a') (f x) (g y)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c x) = Four a b c $ f x

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a `mappend` a') (b `mappend` b') (c `mappend` c') $ f x

data Four' a b = Four' a a b b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' x x') = Four' a a' (f x) (f x')

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty x x
  Four' a b f g <*> Four' a' b' x y = Four' (a `mappend` a') (b `mappend` b') (f x) (g y)
