{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Cipher
import Data.Char (toUpper)

genLetter :: Gen Char
genLetter = elements ['a' .. 'z']

genLetters :: Gen String
genLetters = listOf genLetter

prop_caesarIsomorphism :: Property
prop_caesarIsomorphism = 
  forAll (arbitrary :: Gen Int) $ \ shift ->
  forAll (genLetters) $ \ phrase ->
  (caesarIdentity shift) phrase == map toUpper phrase

prop_vignereIsomorphism :: Property
prop_vignereIsomorphism =
  forAll (genLetters `suchThat` (not . null)) $ \ key ->
  forAll (genLetters) $ \ phrase ->
  vignereIdentity key phrase == map toUpper phrase

caesarIdentity :: Int -> String -> String
caesarIdentity shift = unCaesar shift . caesar shift

vignereIdentity :: Keyphrase -> String -> String
vignereIdentity key = unVignere key . vignere key
  

return []
main :: IO ()
main = do 
  _ <- $quickCheckAll
  return ()

