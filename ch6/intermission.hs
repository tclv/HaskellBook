data TisAnInteger = TisAn Integer deriving Show

instance Eq TisAnInteger where
   (==) (TisAn b) (TisAn a) = a == b

data TwoIntegers = Two Integer Integer deriving Show

instance Eq TwoIntegers where
    (==) (Two a1 b1) (Two a2 b2) = a1 == a2 && b1 == b2

data Pair a = Pair a a deriving Show

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2

data Tuple a b = Tuple a b

instance (Eq a, Eq b) =>  Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2

data Which a = ThisOne a | ThatOne a deriving Show

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False
