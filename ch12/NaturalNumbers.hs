module NaturalNumbers where

import Data.Maybe

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Succ x) = 1 + natToInteger x
natToInteger Zero = 0

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = insertSucc $ integerToNat (x - 1)
    where
      insertSucc :: Maybe Nat -> Maybe Nat
      insertSucc (Just x') = Just (Succ x')
      insertSucc Nothing = Nothing
