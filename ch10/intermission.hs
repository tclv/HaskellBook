import Data.Time
import Data.Scientific
-- 1 b c
-- 2 ( 3 * ( 2 * ( 1 * 1 )))
-- 3 c
-- 4 a
-- 5 
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) False [False, True]
e = foldr ((++) . show ) "" [1..5]
f = foldl const 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldr (flip const) 'z' [1..5]


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr ifTimeAppend []
  where
    ifTimeAppend (DbDate time) acc = time : acc
    ifTimeAppend _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr ifNumAppend []
  where
    ifNumAppend (DbNumber x) acc = x : acc
    ifNumAppend _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber 

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sum numbers) / len
    where numbers = filterDbNumber xs
          len = fromIntegral $ length numbers

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsTakeTwe = take 20 fibs

fibsSmallHun = takeWhile (<100) fibs

fact = scanl1 (*) [1..]
