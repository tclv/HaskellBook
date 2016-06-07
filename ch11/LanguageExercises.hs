module LanguageExercises where

import Data.Char
import Data.List
import Data.List.Split

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capitalizeWord x)) . words

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = unparagraph . map capitalizeWord  . paragraph
  where
    paragraph = splitOn ". "
    unparagraph = intercalate ". "

