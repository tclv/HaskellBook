tensDigit :: Integral a => a -> a
tensDigit x = d
    where (_, d) = x `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
    where (_, d2) = x `divMod` 10

-- b) yes

foldBool :: a -> a -> Bool -> a
foldBool left right switch = case switch of 
    True -> left
    False -> right

foldBool2 :: a -> a -> Bool -> a
foldBool2 left right switch
    | switch == True = left
    | otherwise = right

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)



