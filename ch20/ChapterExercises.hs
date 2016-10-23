module ChapterExercises where

import Data.Monoid
import Data.Foldable

data Constant a b =
  Constant a
  deriving (Eq, Show)


-- (Monoid m, Foldable (Constant a)) => (a -> m) -> ((Constant a) b) -> m
instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap pure . filter f . toList
