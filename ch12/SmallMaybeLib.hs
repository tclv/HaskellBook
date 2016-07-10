module SmallMaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee defVal _ Nothing = defVal
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe defVal Nothing = defVal
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr appJust []
  where
    appJust (Just x) acc = x : acc
    appJust _ acc = acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr appendOrNothing (Just [])
  where
    appendOrNothing (Just x) (Just acc) = Just (x : acc)
    appendOrNothing _ _ = Nothing

