-- 1) a, b, c, d
-- 2) d

-- 3
-- a)

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \z -> z + 1

-- b)
addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x -> \y -> (if x > y then y else x) + 5

-- c)
mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x


-- 1
-- a)
-- b) k1 and k3 have same type
-- c) k3
k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4 - 1), 10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

a1 = dodgy 1 0 == 1
a2 = dodgy 1 1 == 11
a3 = dodgy 2 2 == 22
a4 = dodgy 1 2 == 21
a5 = dodgy 2 1 == 12
a6 = oneIsOne 1 == 11
a7 = oneIsOne 2 == 21
a8 = oneIsTwo 1 == 21
a9 = oneIsTwo 2 == 22
a10 = oneIsOne 3 == 31
a11 = oneIsTwo 3 == 23

-- 1, 2
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    | otherwise = 'F'
    where y = x / 100

-- 3
-- a) no, b) yes, c) no, d) no
pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

-- 4 Equivalent typeclass in list form
-- 5 
-- 6 a) no, b) no, c) yes, d) no
-- 7) Ordering typeclass, number typeclass
numbers :: (Num a, Ord a, Num b) => a -> a
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

