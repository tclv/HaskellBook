module BinaryTree where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f state = case f state of
  Just (stateLeft, nodeVal, stateRight) -> Node (unfold f stateLeft) nodeVal (unfold f stateRight)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold buildCond 0 
  where
    buildCond b = if b < x then Just (b + 1, b, b + 1) else Nothing
