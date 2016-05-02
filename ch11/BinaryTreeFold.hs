module BTFold where

data BT a = 
    Leaf
  | Node (BT a) a (BT a)
  deriving (Eq, Show, Ord)


foldTree :: (a -> b -> b) -> b -> BT a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = accRight
  where
    accNode = f a acc
    accLeft = foldTree f accNode left
    accRight = foldTree f accLeft right


testTree :: BT Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

mapTree' :: (a -> b) -> BT a -> BT b
mapTree' = undefined -- https://www.reddit.com/r/HaskellBook/comments/4czzpp/haskellbookch_11_problems_implementing_maptree_in/


