applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

q1 :: Int
q1 = applyTimes (5 :: Int) (+1) (5 :: Int)
 
q1' :: Int
q1' = (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5
-- 
-- at(5 f 5)
-- f . at 4 f 5
-- f . f . at 3 f 5
-- f . f . f at 2 f 5
-- f . f . f . f at 1 f 5
-- f . f . f . f . f at f 0 5
-- f . f . f . f . f $ b

-- z = applyTimes 5 (\ x -> "(1 + " ++ x ++ ")") "5"


