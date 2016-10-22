import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

main :: IO ()
main = do
  quickBatch $ functor $ (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ applicative $ (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ monad $ (NopeDotJpg :: Nope (Int, String, Int))

  quickBatch $ functor $ (L "a" :: E String (Int, Int, Int))
  quickBatch $ applicative $ (L "a" :: E String (Int, Int, Int))
  quickBatch $ monad $ (L "a" :: E String (Int, Int, Int))

  quickBatch $ functor $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))
  quickBatch $ applicative $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))
  quickBatch $ monad $ (Identity (1, 2, 3) :: Identity (Int, Int, Int))

  quickBatch $ functor $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil
  quickBatch $ monad $ Cons ('a', 'b', 'c') Nil

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= _ = NopeDotJpg

data E e a = 
    L e
  | R a
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary e) => Arbitrary (E e a) where
  arbitrary = oneof [L <$> arbitrary, R <$> arbitrary]

instance (Eq a, Eq e) => EqProp (E e a) where
  (=-=) = eq

instance Functor (E e) where
  fmap _ (L e) = L e 
  fmap f (R x) = R $ f x

instance Applicative (E e) where
  pure = R
  R f <*> R x = R $ f x
  L e <*> _ = L e
  _ <*> L e = L e

instance Monad (E e) where
  -- return = pure -- Not required for min. compl. def.
  R x >>= f = f x
  L e >>= _ = L e

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where 
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  Identity x >>= f = f x

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

instance Monad List where
  xs >>= f = flatMap f xs

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
