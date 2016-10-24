module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Control.Applicative
import Data.Monoid

type T1 = Identity
type T2 = Constant Int
type T3 = Optional
type T4 = List
type T5 = Three Int Int
type T6 = Three' Int
type T7 = S Maybe 
type T8 = Tree

main = do
  quickBatch $ traversable $ (undefined :: T1 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T2 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T3 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T4 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T5 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T6 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T7 (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: T8 (Int, Int, [Int]))

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable (Identity) where
  traverse f (Identity a) = fmap Identity $ f a

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [fmap Yep arbitrary, return Nada]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Functor Optional  where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ Nada    = mempty

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ Nada    = pure Nada

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [liftA2 Cons arbitrary arbitrary, return Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap f (Cons x xs) = f x `Cons` fmap f xs
  fmap _ Nil         = Nil

instance Foldable List where
  foldMap f (Cons x xs) = f x <> foldMap f xs
  foldMap _ Nil         = mempty

instance Traversable List where
  traverse f = foldr (liftA2 Cons) (pure Nil) . fmap f 

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b $ f x

instance Foldable (Three a b) where
  foldMap f (Three a b x) = f x

instance Traversable (Three a b) where
  traverse f (Three a b x) = (Three a b) <$> f x

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a x x') = Three' a (f x) (f x')

instance Foldable (Three' a) where
  foldMap f (Three' a x x') = f x <> f x'

instance Traversable (Three' a) where
  traverse f (Three' a x x') = Three' a <$> f x <*> f x'

data S n a = S (n a) a
  deriving (Eq, Show)

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = liftA2 S arbitrary arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) $ (f a) 

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*>  f a

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [return $ Empty, Leaf <$> arbitrary, liftA3 Node arbitrary arbitrary arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'

instance Traversable Tree where
  traverse f Empty         = pure Empty
  traverse f (Leaf a)      = Leaf <$> f a
  traverse f (Node n x n') = Node <$> traverse f n <*> f x <*> traverse f n'
