type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
module Cipher (caesar, unCaesar, vignere, unVignere) where

import Data.Char

type Keyphrase = String

shiftChar :: Int -> Char -> Char
shiftChar 0 x = x
shiftChar shift x
  | ord x + shift > ord 'Z' = shiftChar (shift - 26) x
  | ord x + shift < ord 'A' = shiftChar (shift + 26) x
  | otherwise               = chr $ ord x + shift

caesar :: Int -> String -> String
caesar shift code = map (shiftChar shift) code

unCaesar :: Int -> String -> String
unCaesar shift code = map (shiftChar $ negate shift) code

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

unVignere :: Keyphrase -> String -> String
unVignere key code = map (uncurry shiftChar) negShiftAmounts
  where
    upperCode              = map toUpper code
    upperKey               = map toUpper key
    encodeAmounts          = shiftAmounts (cycle upperKey) upperCode
    negShiftAmounts        = map negShiftFunction encodeAmounts
    negShiftFunction (a,b) = (negate a, b)
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name: " ++ show name ++ " Age: " ++ show age

splitInput :: String -> (Name, Age)
splitInput inp = (unwords $ init split, (read $ last split))
  where
    split = words inp
    

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter person with following format [name age]"
  nameAndAge <- splitInput <$> getLine
  case (uncurry mkPerson) nameAndAge of
    Right p -> putStrLn $ "Yay! " ++ show p
    Left e  -> putStrLn $ "Error! " ++ show e

main :: IO ()
main = gimmePerson
