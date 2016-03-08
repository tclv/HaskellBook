module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar shift x
  | ord x + shiftMod > ord 'z' = shiftChar shiftMod (chr $ ord x - 26)
  | ord x + shiftMod < ord 'a' = shiftChar shiftMod (chr $ ord x + 26)
  | otherwise                  = chr $ ord x + shiftMod
  where
    shiftMod = shift `mod` 26



caesar :: Int -> String -> String
caesar shift code = map (shiftChar shift) code

unCaesar :: Int -> String -> String
unCaesar shift code = map (shiftChar $ negate shift) code
