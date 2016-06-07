module Phone where

import Data.Char

data Option = Digit Char | Capitalize deriving (Eq, Show)

data Button = Button ButtonIdentifier [Option] deriving (Eq, Show)

data ButtonIdentifier = One | Two | Three | Four
                      | Five | Six | Seven | Eight
                      | Nine | Star | Zero | Bracket
                      deriving (Eq, Show)

type Presses = Int

newtype PhonePad = PhonePad [Button] deriving (Eq, Show)

phonePad :: PhonePad
phonePad = PhonePad [one, two, three
                    ,four, five, six
                    ,seven, eight, nine
                    ,star, zero, bracket]
  where
    one = Button One (buildDigitList ['1'])
    two = Button Two (buildDigitList ['a', 'b', 'c', '2'])
    three = Button Three (buildDigitList ['d', 'e', 'f', '3'])
    four = Button Four (buildDigitList ['g', 'h', 'i', '4'])
    five = Button Five (buildDigitList ['j', 'k', 'l', '5'])
    six = Button Six (buildDigitList ['m', 'n', 'o', '6'])
    seven = Button Seven (buildDigitList ['p', 'q', 'r', 's', '7'])
    eight = Button Eight (buildDigitList ['t', 'u', 'v', '8'])
    nine = Button Nine (buildDigitList ['w', 'x', 'y', 'z', '9'])
    star = Button Star [Capitalize]
    zero = Button Zero (buildDigitList ['+', ' ', '0'])
    bracket = Button Bracket (buildDigitList ['.', ',', '\n'])

reversePhonePad (PhonePad buttons) = lookUpify buildList
  where
    lookUpify = map (\(x, y, z) -> (x, (y, z))) 
    buildList = concat $ map (\(Button btype options) -> zip3 options (repeat btype) [1..]) buttons

buildDigitList :: [Char] -> [Option]
buildDigitList = map Digit


charToTaps :: PhonePad -> Char -> Maybe [(ButtonIdentifier, Presses)]
charToTaps phonePad x = sequence $ map (flip lookup $ revPad) options 
  where 
    options = charToOption x
    revPad = reversePhonePad phonePad


stringToTaps :: PhonePad -> [Char] -> Maybe [(ButtonIdentifier, Presses)]
stringToTaps phonepad x = fmap concat $ sequence $ map (charToTaps phonePad) x


charToOption :: Char -> [Option]
charToOption x 
  | isUpper x = Capitalize : Digit (toLower x) : []
  | otherwise = Digit x : []

convo :: [String] 
convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]
