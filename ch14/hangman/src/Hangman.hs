module Hangman where

import Data.List (intersperse)
data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed guessesLeft) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed ++ " Guesses left: " ++ show guessesLeft

numberOfGuessesInit :: Int
numberOfGuessesInit = 7

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) guess = guess `elem` word

missedGuess :: Puzzle -> Char -> Puzzle
missedGuess puzzle guess = Puzzle w f m (noG - 1)
  where
    (Puzzle w f m noG) = fillInCharacter puzzle guess

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessedSoFar _) guess = guess `elem` guessedSoFar

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) [] numberOfGuessesInit

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s noG) c =
  Puzzle word newFilledInSoFar (c : s) noG
  where 
    zipper guessed wordChar guessChar = 
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "Your already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (missedGuess puzzle guess)
