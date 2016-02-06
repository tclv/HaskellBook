import Data.List (sort)
-- 1

-- a)
i :: Num a => a
i = 1

-- b) Needs to be Num, no instance arising from literal
-- i' :: a
-- i' = 1
-- GHC defaults to Integer (Haskell Report)

-- 2
-- a)
f :: Float
f = 1.0

-- b) Does not compile. Highest class is Fractional. 
-- Defaults to Double
-- f' :: Num a => a
-- f' = 1.0

-- 3
-- a)
g :: Float
g = 1.0

-- b) Double Default. Compiles because 1.0 is instance of fractional.
g' :: Fractional a => a
g' = 1.0

-- 4
-- a)
h :: Float
h = 1.0

-- b) 1/1 is a real fraction. Default Double
h' :: RealFrac a => a 
h' = 1.0

-- 5
-- a)
freud :: a -> a
freud x = x

-- b) typechecks (highest possible sign: a -> a)
freudcheck :: Ord a => a -> a
freudcheck x = x

-- 6
-- a)
freud' :: a -> a
freud' x = x

--b) Typechecks (more specific than a -> a again)
freudcheck' :: Int -> Int
freudcheck' x = x

-- 7
-- a)
myX :: Int
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- b) Does not typecheck as return is Int so input also
-- sigmund' :: a -> a
-- sigmund' x = myX

-- 8
-- a) idem 7a
-- b) Does not typecheck as myX is defined as an Int
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- 9
-- a)
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- b) Typechecks Int implements Ord. Normally implements Ord
jung' :: [Int] -> Int
jung' xs = head (sort xs)

-- 10
-- a)
young :: [Char] -> Char
young xs = head (sort xs)

-- b) Typechecks. 
young' :: Ord a => [a] -> a
young' xs = head (sort xs)

-- 11
-- a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- b) Fails as sort typechecks for Char and Ord is too broad
-- signifier' :: Ord a => [a] -> a
-- signifier' xs = head (mySort xs)


