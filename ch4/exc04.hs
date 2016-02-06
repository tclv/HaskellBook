awesome = ["Papuchon", "curry", "Haskell"] 
alsoAwesome = ["Quake", "The Simons"] 
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then (negate x) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))


x = (+)
reading xs = w `x` 1
    where w = length xs

z = \ x -> x

first = \ (x:xs) -> x

fst' (a,b) = a
