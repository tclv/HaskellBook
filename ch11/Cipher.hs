module Cipher where

import Data.Char

type Keyphrase = String

shiftChar :: Int -> Char -> Char
shiftChar 0 x = x
shiftChar shift x
  | ord x + shift > ord 'Z' = shiftChar (shift - 26) x
  | ord x + shift < ord 'A' = shiftChar (shift + 26) x
  | otherwise               = chr $ ord x + shift


vignere :: Keyphrase -> String -> String
vignere key inp = map (uncurry shiftChar) encodeAmounts
  where 
    upperInp      = map toUpper inp
    upperKey      = map toUpper key
    encodeAmounts = shiftAmounts (cycle upperKey) upperInp

shiftAmounts :: Keyphrase -> String -> [(Int, Char)]
shiftAmounts _ [] = []
shiftAmounts [] _ = []
shiftAmounts key@(x:xs) (y:ys)
  | isAsciiUpper y = (ord x - ord 'A', y) : shiftAmounts xs ys
  | otherwise      = (0, y) : shiftAmounts key ys

unvignere :: Keyphrase -> String -> String
unvignere key code = map (uncurry shiftChar) negShiftAmounts
  where
    upperCode              = map toUpper code
    upperKey               = map toUpper key
    encodeAmounts          = shiftAmounts (cycle upperKey) upperCode
    negShiftAmounts        = map negShiftFunction encodeAmounts
    negShiftFunction (a,b) = (negate a, b)
