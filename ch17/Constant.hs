module Constant where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure = const $ Constant mempty 
  Constant x <*> Constant y = Constant $ x <> y

main :: IO ()
main = quickBatch $ applicative $ (Constant "b" :: Constant String (String, String, String))

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq
