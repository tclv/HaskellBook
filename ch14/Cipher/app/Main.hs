module Main where

import Cipher

import Data.Char



main :: IO ()
main = do
  putStrLn "Write c, for Caeser and v for vignere"
  method <- getLine
  putStrLn "Write d for decode or c for code"
  mode <- getLine
  putStrLn "Present key"
  key <- getLine
  putStrLn "Present line to be coded/decoded"
  phrase <- getLine
  putStrLn $ case (method, mode) of
                ("c", "c") -> caesar (read key) phrase
                ("c", "d") -> unCaesar (read key) phrase
                ("v", "c") -> vignere key phrase
                ("v", "d") -> unVignere key phrase
                _ -> error ("Incorrect input arguments")


