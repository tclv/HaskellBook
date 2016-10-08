module First where

import Optional
import Data.Monoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
     a <- arbitrary
     return (First' a)
    
instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) _ = First' (Only a)
  mappend (First' Nada) b     = b

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend = 
     First' String 
  -> First' String 
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (mempty <> a ) == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
