chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == f a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x a = (fromIntegral x) + f a
