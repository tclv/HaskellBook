divideBy :: (Num a, Ord a) => a -> a -> a
divideBy num denom
    | num >= denom = 1 + divideBy (num - denom) denom
    | otherwise = 0


sumToN :: (Num a, Eq a) => a -> a
sumToN n
    | n /= 0 = n + sumToN (n - 1)
    | otherwise = 0

multRecursive :: (Integral a) => a -> a -> a
multRecursive a b 
    | a > 0 = b + multRecursive (a - 1) b
    | a < 0 = (negate b) + multRecursive (a + 1) b
    | otherwise = 0


mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 $ mc91 (n + 11)
