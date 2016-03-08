myEnumFromTo :: (Enum a) => a -> a -> [a]
myEnumFromTo beg end
    | begInd > endInd = []
    | begInd == endInd = beg : []
    | otherwise = beg : myEnumFromTo (succ beg) end
        where begInd = fromEnum beg
              endInd = fromEnum end

eftBool :: Bool -> Bool -> [Bool]
eftBool beg end = myEnumFromTo beg end

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd beg end = myEnumFromTo beg end

eftInt :: Int -> Int -> [Int]
eftInt beg end = myEnumFromTo beg end

eftChar :: Char -> Char -> [Char]
eftChar beg end = myEnumFromTo beg end

