module RandomGenerator where

import Test.QuickCheck


data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

equalProbability :: Gen Fool
equalProbability = do
  elements [Fulse, Frue]

twoThirdProb :: Gen Fool
twoThirdProb = do
  frequency [ (1, return $ Frue)
            , (2, return $ Fulse) ]

instance Arbitrary Fool where
  arbitrary = twoThirdProb
  -- arbitrary = equalProbability
  -- depending on which as default
