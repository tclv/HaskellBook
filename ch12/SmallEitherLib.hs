module SmallEitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr appendLeft []
  where
    appendLeft (Left x) list = x : list
    appendLeft _ list = list

rights' :: [Either a b] -> [b]
rights' = foldr appendRight []
  where
    appendRight (Right x) list = x : list
    appendRight _ list = list

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr append ([], [])
  where
    append (Left x) (leftL, rightL) = (x : leftL, rightL)
    append (Right x) (leftL, rightL) = (leftL, x : rightL)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _ ) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (const Nothing) (Just . f) x

