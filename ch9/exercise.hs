myEnumFromTo :: (Enum a) => a -> a -> [a]
myEnumFromTo beg end
    | begInd > endInd = []
    | otherwise = beg : myEnumFromTo (succ beg) end
        where begInd = fromEnum beg
              endInd = fromEnum end
