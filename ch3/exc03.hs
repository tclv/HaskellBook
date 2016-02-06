module Reverse where

area d = pi * (r * r)
    where r = d / 2


exclamate :: String -> String
exclamate xs = xs ++ "!"

fifthLetter :: String -> String
fifthLetter = drop 4 . take 5

lastEight :: String -> String
lastEight xs = drop n xs
    where n = length xs - 8


thirdLetter :: String -> Char
thirdLetter xs = xs !! 3

getOffset :: Int -> Char
getOffset offset = str !! offset
    where str = "Curry is awesome!"

rvrs :: String -> String
rvrs = unwords . reverse . words

main :: IO ()
main = print $ rvrs "Curry is awesome"
