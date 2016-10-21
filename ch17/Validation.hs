module Validation where

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  Failure a <*> Failure b = Failure $ a `mappend` b
  Success f <*> Success a = Success $ f a 
  Failure a <*> _         = Failure a
  _         <*> Failure a = Failure a


instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Success <$> arbitrary, Failure <$> arbitrary]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

main = do
  quickBatch $ applicative $ (Success ("a", "b", "c") :: Validation String (String, String, String))

