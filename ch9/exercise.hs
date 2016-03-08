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

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f 

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = x == y || myElem y xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

squish :: [[a]] -> [a]
squish []       = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x : xs) = go cmp xs x
  where go _ [] acc = acc
        go cmp (x:xs) acc
          | acc `cmp` x == GT = go cmp xs acc
          | acc `cmp` x /= GT = go cmp xs x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x : xs) = go cmp xs x
  where go _ [] acc = acc
        go cmp (x:xs) acc
          | acc `cmp` x == LT = go cmp xs acc
          | acc `cmp` x /= LT = go cmp xs x

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
