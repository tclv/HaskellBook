module Either where

data E e a = 
    L e
  | R a
  deriving (Eq, Show)

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
