module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f cur = cur : myIterate f (f cur)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f state = case f state of
  Just (val, newState) -> val : myUnfoldr f newState
  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x' -> Just (x', f x')) x
