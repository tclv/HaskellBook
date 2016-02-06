a = (* 9) 6
b = head [(0, "doge"), (1, "Kitteh")]
c = head [(0 :: Integer, "doge"), (1, "Kitteh")]
d = if False then True else False
e = length [1,2,3,4,5]
f = (length [1,2,3,4]) > (length "Tacocat")


x = 5
y = x + 5
w = y * 10

z y = y * 10

f' = 4 / y

x' = "Julie"
y' = " <3 "
z' = "Haskell"
f'' = x' ++ y' ++ z'


bigNum = (^) 5 $ 10
wahoo = bigNum

xpr = print
ypr = print "woohoo!"
zpr = xpr "hello world"

aaa = (+)
bbb = 5
ccc = aaa 10
ddd = ccc 200

aaaa = 12 + bbbb
bbbb = 10000 * aaaa


functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

cfunc :: a -> b -> a
cfunc x y = x

cfunc' :: a -> b -> b
cfunc' x y = y

r :: [a] -> [a]
r (_:xs) = xs

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

a'' :: (a -> c) -> a -> a
a'' f x = x

a''' :: (a -> b) -> a -> b
a''' f x = f x
