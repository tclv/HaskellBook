{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Test.QuickCheck


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <>c)

type Associativity x = x -> x -> x -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Associativity (Identity String))
  quickCheck (semigroupAssoc :: Associativity (Two String String))
  quickCheck (semigroupAssoc :: Associativity (Three String String String))
  quickCheck (semigroupAssoc :: Associativity (Four String String String String))
  quickCheck (semigroupAssoc :: Associativity BoolConj)
  quickCheck (semigroupAssoc :: Associativity BoolDisj)
  quickCheck (semigroupAssoc :: Associativity (Or String String))

newtype Identity a = Identity a deriving (Eq, Show, Semigroup, Arbitrary)

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two w z) = Two (x <> w) (y <> z)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, 
          Arbitrary b, 
          Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z
  
instance (Semigroup a, 
          Semigroup b, 
          Semigroup c) => Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

data Four a b c d = Four a b c d deriving (Eq, Show)


instance (Arbitrary a, 
          Arbitrary b, 
          Arbitrary c,
          Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

instance (Semigroup a, 
          Semigroup b, 
          Semigroup c,
          Semigroup d) => Semigroup (Four a b c d) where
 (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)


newtype BoolConj = BoolConj Bool deriving (Eq, Show, Arbitrary)

instance Semigroup BoolConj where
  BoolConj False <> BoolConj _ = BoolConj False
  BoolConj True <> BoolConj a = BoolConj a

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show, Arbitrary)

instance Semigroup BoolDisj where
  BoolDisj True <> BoolDisj _ = BoolDisj True
  BoolDisj False <> BoolDisj a = BoolDisj a

data Or a b =
    Fst a
  | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof $ map return [Fst x, Snd y]

instance Semigroup (Or a b) where
  Fst a <> z = z
  Snd a <> _ = Snd a
  



