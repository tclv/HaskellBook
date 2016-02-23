module WordNumber where

import Data.List (intersperse)
import Data.Char

digitToWord :: Int -> String
digitToWord n
    | n >= 0 && n < 10 = (chr ((ord '0') + n)) : []


digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = (digits quot) ++ [rem]
        where (quot, rem) = quotRem n 10

wordNumber :: Int -> String
wordNumber n = concat $ map digitToWord $ digits n


