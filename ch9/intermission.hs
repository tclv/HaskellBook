myWords :: String -> [String]
myWords "" = []
myWords inp = beforeSpace : afterSpace
    where notSpace = \x -> x /= ' '
          stillSpace = \x -> x == ' '
          beforeSpace = takeWhile notSpace inp
          afterSpace = myWords . (dropWhile stillSpace) . dropWhile notSpace $ inp


partitioner :: Eq a => a -> [a] -> [[a]]
partitioner sep [] = []
partitioner sep inp = beforeSpace : afterSpace
    where notSep = \x -> x /= sep
          isSep = \x -> x == sep
          beforeSpace = takeWhile notSep inp
          afterSpace = partitioner sep . dropWhile isSep . dropWhile notSep $ inp


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

len = length [(x,y) | (x, y) <- zip mySqr myCube, x < 50 && y < 50]


-- normal form
-- 1 1
-- 2 2
-- 3 3
-- 4 3
-- 5 3
-- 6 3
-- 7 2
