{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module SG where

import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)
import Text.Show.Functions

type Associativity x = x -> x -> x -> Bool
type FunctionAssociativity x c = x -> x -> x -> c -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => Associativity m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupCombineAssoc :: (Eq b, Semigroup b) => FunctionAssociativity (Combine a b) a
semigroupCombineAssoc f g h c = unCombine ((f <> g) <> h) c == unCombine (f <> (g <> h)) c

semigroupCompAssoc :: Eq a => FunctionAssociativity (Comp a) a
semigroupCompAssoc f g h c = unComp ((f <> g) <> h) c == unComp (f <> (g <> h)) c


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Associativity (Identity String))
  quickCheck (semigroupAssoc :: Associativity (Two String String))
  quickCheck (semigroupAssoc :: Associativity (Three String String String))
  quickCheck (semigroupAssoc :: Associativity (Four String String String String))
  quickCheck (semigroupAssoc :: Associativity BoolConj)
  quickCheck (semigroupAssoc :: Associativity BoolDisj)
  quickCheck (semigroupAssoc :: Associativity (Or String String))
  quickCheck (semigroupCombineAssoc :: FunctionAssociativity (Combine Int String) Int)
  quickCheck (semigroupCompAssoc :: FunctionAssociativity (Comp String) String)
  quickCheck (semigroupAssoc :: Associativity (Validation String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateRight String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateBoth String String))

newtype Identity a = Identity a deriving (Eq, Show, Arbitrary)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity $ x <> y

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two w z = Two (x <> w) (y <> z)

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
  BoolConj True  <> BoolConj a = BoolConj a

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
  
newtype Combine a b =
    Combine { unCombine :: a -> b }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return $ Combine a 

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

instance Show (Combine a b) where
  show (Combine f) = "Combine " ++ show f

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance Show (Comp a) where
  show (Comp f) = "Comp " ++ show f

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ map return [Failure a, Success b]

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure $ x <> y
  Failure x <> Success y = Failure x
  Success x <> y = y
  
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show, Arbitrary)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight x <> AccumulateRight x' = 
    let res = 
          case (x, x') of
                (Success x, Success y) -> Success $ x <> y
                (Success x, Failure y) -> Failure y
                (Failure x, _)         -> Failure x
             in AccumulateRight res

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show, Arbitrary)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth x <> AccumulateBoth x' = 
    let res = 
          case (x, x') of
                (Success x, Success y) -> Success $ x <> y
                (Failure x, Failure y) -> Failure $ x <> y
                (Failure x, _)         -> Failure x
                (_, Failure y)         -> Failure y
             in AccumulateBoth res
