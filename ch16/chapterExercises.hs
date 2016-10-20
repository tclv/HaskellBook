{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

-- Determine if valid functor
--1. No (need * -> *)
--2. Yes
--3. Yes
--4. 
newtype Mu f = InF { outF :: f (Mu f) } -- Some kind of catamorphism, review later
--4. :k Mu = (* -> *) -> *
--no instance available

-- instance Functor Mu where -- Won't compile
--   fmap = undefined

data D = -- kind *
  D (Array Word Word) Int Int

-- no instance functor possible due to kind *

-- Rearrange argument to type constructor for functor to work
-- 1.

data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First ( f a)
  fmap f (Second b) = Second b

-- 2.

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write down functor instances for the following datatypes:
-- 1.

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  {-fmap _ x = x -- Doesnt work, cant deduce the change in the b of Quant a b-}

-- 2.

data K a b
  = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K $ f x

-- 4.

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

-- 5.

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut $ fmap g x

-- 6.

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7.

data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething x y) = IgnoreSomething x $ fmap f y

-- 8.

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

-- 9.

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

-- 10.

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x a) = Print x $ f a
  fmap f (Read g) = Read $ fmap f g

  
