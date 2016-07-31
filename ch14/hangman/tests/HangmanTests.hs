module Main where

import Test.Hspec
import Hangman
import Data.List (repeat)

testPuzzle@(Puzzle word freshGuess emptyCharLog freshNoGuess) = freshPuzzle "haskell"
missCharPuzzle@(Puzzle _ missGuess missCharLog missNoGuess) = fillInCharacter testPuzzle 'x'

hitCharPuzzle@(Puzzle _ hitGuess hitCharLog hitNoGuess) = fillInCharacter testPuzzle 'a'


main :: IO ()
main = hspec $ do
  describe "HangmanMiss" $ do
    it "Should add a letter to played letter list if letter not in word" $ do
      missCharLog `shouldBe` ['x']
    it "Should not update filled in list on miss" $ do
      missGuess `shouldBe` freshGuess
  describe "HangmanHit" $ do
    it "Should add a letter to played letter list if letter in word" $ do
      hitCharLog `shouldBe` ['a']
    it "Should update filled in list on hit" $ do
      hitGuess `shouldBe` [Nothing, Just 'a'] ++ take 5 (repeat Nothing)
      
