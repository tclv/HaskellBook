module Mon where

import SG
import Test.QuickCheck
import Data.Monoid hiding ((<>))
import Data.Semigroup hiding (mappend)
import Text.Show.Functions


type LeftIdentity x = x -> Bool
type RightIdentity x = x -> Bool
type FunctionIdentity x a = x -> a -> Bool

monoidLeftIdentity :: (Monoid m, Semigroup m, Eq m) => LeftIdentity m
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Monoid m, Semigroup m, Eq m) => RightIdentity m
monoidRightIdentity x = x <> mempty == x

monoidLeftIdentityCombine :: (Eq b, Semigroup b, Monoid b) => FunctionIdentity (Combine a b) a
monoidLeftIdentityCombine f c = unCombine (mempty <> f) c == unCombine f c

monoidRightIdentityCombine :: (Eq b, Semigroup b, Monoid b) => FunctionIdentity (Combine a b) a
monoidRightIdentityCombine f c = unCombine (f <> mempty) c == unCombine f c

monoidLeftIdentityComp :: (Eq a, Semigroup a, Monoid a) => FunctionIdentity (Comp a) a
monoidLeftIdentityComp f c = unComp (mempty <> f) c == unComp f c

monoidRightIdentityComp :: (Eq a, Semigroup a, Monoid a) => FunctionIdentity (Comp a) a
monoidRightIdentityComp f c = unComp (f <> mempty) c == unComp f c

monoidMemAssoc :: (Eq a, Eq s, Monoid a) => FunctionAssociativity (Mem s a) s
monoidMemAssoc f g h c = runMem ((f `mappend` g) `mappend` h) c == runMem (f `mappend` (g `mappend` h)) c

monoidLeftIdentityMem :: (Eq a, Eq s, Monoid a) => FunctionIdentity (Mem s a) s
monoidLeftIdentityMem f c = runMem (mempty `mappend` f) c == runMem f c

monoidRightIdentityMem :: (Eq a, Eq s, Monoid a) => FunctionIdentity (Mem s a) s
monoidRightIdentityMem f c = runMem (f `mappend` mempty) c == runMem f c

main :: IO ()
main = do
  quickCheck (semigroupAssoc             :: Associativity (Identity String))
  quickCheck (monoidLeftIdentity         :: LeftIdentity (Identity String))
  quickCheck (monoidRightIdentity        :: RightIdentity (Identity String))

  quickCheck (semigroupAssoc             :: Associativity (Two String String))
  quickCheck (monoidLeftIdentity         :: LeftIdentity (Two String String))
  quickCheck (monoidRightIdentity        :: RightIdentity (Two String String))

  quickCheck (semigroupAssoc             :: Associativity BoolConj)
  quickCheck (monoidLeftIdentity         :: LeftIdentity BoolConj)
  quickCheck (monoidRightIdentity        :: RightIdentity BoolConj)

  quickCheck (semigroupAssoc             :: Associativity BoolDisj)
  quickCheck (monoidLeftIdentity         :: LeftIdentity BoolDisj)
  quickCheck (monoidRightIdentity        :: RightIdentity BoolDisj)

  quickCheck (semigroupAssoc             :: Associativity BoolDisj)
  quickCheck (monoidLeftIdentity         :: LeftIdentity BoolDisj)
  quickCheck (monoidRightIdentity        :: RightIdentity BoolDisj)

  quickCheck (semigroupCombineAssoc      :: FunctionAssociativity (Combine String String) String)
  quickCheck (monoidLeftIdentityCombine  :: FunctionIdentity (Combine String String) String)
  quickCheck (monoidRightIdentityCombine :: FunctionIdentity (Combine String String) String)

  quickCheck (semigroupCompAssoc         :: FunctionAssociativity (Comp String) String)
  quickCheck (monoidLeftIdentityComp     :: FunctionIdentity (Comp String) String)
  quickCheck (monoidRightIdentityComp    :: FunctionIdentity (Comp String) String)

  quickCheck (monoidMemAssoc             :: FunctionAssociativity (Mem Int String) Int)
  quickCheck (monoidLeftIdentityMem      :: FunctionIdentity (Mem Int String) Int)
  quickCheck (monoidRightIdentityMem     :: FunctionIdentity (Mem Int String) Int)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    s <- arbitrary
    return $ Mem s

instance Show (Mem a b) where
  show (Mem f) = "Mem " ++ show f

instance Monoid a => Monoid (Mem s a) where
  mappend (Mem f1) (Mem f2) = Mem $ \s ->
    let (a', s')   = f1 s
        (a'', s'') = f2 s'
    in (a' `mappend` a'', s'')
  mempty = Mem $ \s -> (mempty, s)
