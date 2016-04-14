module Jammin where
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam {fruit :: Fruit 
      , jars :: Int }
  deriving (Eq, Show, Ord)


-- 3 |JamJars| = 3 * |Int| = muchos
-- 5

row1 :: JamJars
row1 = Jam Peach 5
row2 :: JamJars
row2 = Jam Plum 3
row3 :: JamJars
row3 = Jam Blackberry 4
row4 :: JamJars
row4 = Jam Plum 1
row5 :: JamJars
row5 = Jam Apple 6
row6 :: JamJars
row6 = Jam Peach 2

allJam :: [JamJars]
allJam = [row1, row2, row3, row4, row5, row6]

-- 6
numberOfJars :: [JamJars] -> Int
numberOfJars = sum . map jars

-- 7
mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\x y -> compare (jars x) (jars y))

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortAllJams :: [JamJars] -> [JamJars]
sortAllJams = sortBy compareKind

groupJams :: [JamJars] -> [[JamJars]]
groupJams = groupBy (\x y -> compareKind x y == EQ) . sortAllJams

