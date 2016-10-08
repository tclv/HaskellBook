module Optional where

import Test.QuickCheck
import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) (Only b) = Only (a <> b)
  mappend (Only a) Nada = Only a
  mappend Nada Nada = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = arbitrary >>= \a -> frequency [(1, return Nada), (1, return (Only a))]
