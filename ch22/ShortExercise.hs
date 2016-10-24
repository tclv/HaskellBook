import Data.Char

import Control.Applicative

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = liftA2 (,) id composed

tupledM :: String -> (String, String)
tupledM = do
  a <- id
  b <- composed
  return (a, b)

tupledM' :: String -> (String, String)
tupledM' = id >>= (\x -> composed >>= (\y -> return $ (x, y)))

