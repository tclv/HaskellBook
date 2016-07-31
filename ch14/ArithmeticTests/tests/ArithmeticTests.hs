{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (sort)
import Test.QuickCheck
import Arithmetic
import Text.Show.Functions



prop_halfIdentity :: Property
prop_halfIdentity = forAll 
  (arbitrary :: Gen Double) 
  (\x -> halfIdentity x == x)

prop_listOrdered :: Property
prop_listOrdered = forAll
  (arbitrary :: Gen ([Int]))
  (listOrdered . sort)

prop_plusAssociative :: Property
prop_plusAssociative = forAll
  (arbitrary :: Gen Int)
  plusAssociative

prop_plusCommutative :: Property
prop_plusCommutative = forAll
  (arbitrary :: Gen Int)
  plusCommutative

prop_multAssociative :: Property
prop_multAssociative = forAll
  (arbitrary :: Gen Int)
  multAssociative

prop_multCommutative :: Property
prop_multCommutative = forAll
  (arbitrary :: Gen Int)
  multCommutative

prop_quotRemLaws :: Property
prop_quotRemLaws = 
  forAll nonZeroInt $ \x -> 
  forAll nonZeroInt $ \y -> 
  (quot x y) * y + (rem x y) == x

prop_divModLaws :: Property
prop_divModLaws = 
  forAll nonZeroInt $ \x -> 
  forAll nonZeroInt $ \y -> 
  (div x y) * y + (mod x y) == x

prop_powerAssociative :: Property
prop_powerAssociative = forAll
  (arbitrary :: Gen Int)
  powerAssociative

prop_powerCommutative :: Property
prop_powerCommutative = forAll
  (arbitrary :: Gen Int)
  powerCommutative

prop_reverseIdentity :: Property
prop_reverseIdentity = forAll
  (arbitrary :: Gen [Int])
  reverseIdentity

-- Taken from QuickCheck Documentation
prop_ComposeAssoc :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Int -> Bool
prop_ComposeAssoc f g h x =
  ((f . g) . h) x == (f . (g . h)) x

-- Analogous to quickCheck compose example
prop_dollarSignLaw :: (Int -> Char) -> Int -> Bool
prop_dollarSignLaw f a = dollarSignLaw f a

prop_foldrCons :: Property
prop_foldrCons = forAll 
  (arbitrary :: Gen [Int]) $
  \ x y -> foldr (:) x y == (flip (++)) x y

prop_foldrConcat :: Property
prop_foldrConcat = forAll 
  (arbitrary :: Gen [[Int]]) $
  \ x -> foldr (++) [] x == concat x

prop_takeLength :: Property
prop_takeLength = 
  forAll (positiveInt) $ \ x ->
  forAll (arbitrary :: Gen [Char]) $ \ y ->
  takeLength x y

prop_readShowIsomorphism :: Property
prop_readShowIsomorphism = forAll
  (arbitrary :: Gen Int) $ 
  \ x -> (read . show) x == x

-- sqrt(-3 * -3) = 3 != 3
-- besides this, floating point is not an exact representation due to the shifting for addition of numbers with different exponents. Guard digits help, for small differences
prop_squreIdentity :: Property
prop_squreIdentity = forAll
  (arbitrary :: Gen Double) $
  \ x -> squareIdentity x == id x

prop_capitalizeIdemPot :: Property
prop_capitalizeIdemPot = forAll
  (arbitrary :: Gen String) $
  \x -> 
    let onceApl = capitalizeWord x
        twiceApl = twice capitalizeWord x
        friceApl = fourTimes capitalizeWord x
    in (onceApl == twiceApl) && (twiceApl == friceApl) 
          -- once, twice, thrice, frice??

prop_sortIdemPot :: Property
prop_sortIdemPot = forAll
  (arbitrary :: Gen [Int]) $
  \x ->
    let onceApl = sort x
        twiceApl = twice sort x
        friceApl = fourTimes sort x
    in (onceApl == twiceApl) && (twiceApl == friceApl)

nonZeroInt :: Gen Int
nonZeroInt = (arbitrary :: Gen Int) `suchThat` nonZero
  where 
    nonZero = (/=) 0

positiveInt :: Gen Int
positiveInt = (arbitrary :: Gen Int) `suchThat` ((flip (>)) 0)

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do 
  _ <- runTests
  return ()
    

