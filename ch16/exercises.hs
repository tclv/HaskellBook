import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Applicative
-- Kind exercises
-- 1. a is *
-- 2. b is * -> *, T = * -> *
-- 3. c is * -> * -> *


-- fmap1 :: Functor f => (m -> n) -> f m -> f n
-- fmap1 = undefind
--
-- fmap2 :: Functor g => (x -> y) -> g x -> g y
-- fmap2 = undefined

-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- ((f x -> f y) -> f g x -> f g y) -> ((x -> y) -> f x -> f y) -> (x -> y) -> f g x -> f g y

a_ = fmap (+1) $ read "[1]" :: [Int]
b_ = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c_ = fmap (*2) (\x -> x -2)
d_ = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e_ :: IO Integer
e_ = let ioi = readIO "1" :: IO Integer
         changed = fmap (fmap read (fmap ("123" ++) show)) ioi
     in fmap (*3) changed


type ComposeLaw f a b c = f a -> Fun a b -> Fun b c -> Bool

functorCompose :: (Eq (f c), Functor f) => ComposeLaw f a b c
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x ) == (fmap g . fmap f $ x)


main = do
  quickCheck (functorCompose :: ComposeLaw Identity Int String Char)
  quickCheck (functorCompose :: ComposeLaw Pair Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Two Int) Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Three Int Int) Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Three' Int) Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Four Int Int Int) Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Four' Int) Int String Char)
  quickCheck (functorCompose :: ComposeLaw Possibly Int String Char)
  quickCheck (functorCompose :: ComposeLaw (Sum Int) Int String Char)

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary 

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
  fmap f (Two y x) = Two y (f x)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
  fmap f (Four a b c x) = Four a b c (f x)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four' a) where
  fmap f (Four' a b c x) = Four' a b c (f x)

data Possibly a =
    LolNope
  | Yeppers a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = frequency [(8, fmap Yeppers arbitrary), (2, return LolNope)]
    
instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [fmap First arbitrary, fmap Second arbitrary]

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b
