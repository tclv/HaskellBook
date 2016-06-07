module AsPatterns where

-- elementsOf
-- isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
-- isSubsequenceOf xs ys = foldr ((&&) . inSequence) True xs
--   where
--     inSequence = flip elem $ ys

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xss@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xss ys
