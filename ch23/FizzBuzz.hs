
import Control.Monad
import Control.Monad.Trans.State

import qualified Data.DList as DL

import Data.Foldable

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n




fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (mapM_ addR' [to, to-1 .. from]) []

addR' :: Integer -> State ([String]) ()
addR' n = do
  xs <- get 
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = do 
  let a = toList $ fizzbuzzList $ [1 .. 100]
      b = fizzbuzzFromTo 1 100

  print $ a == b

{-main :: IO ()-}
{-main = traverse_ (putStrLn . fizzBuzz) [1..100]-}
