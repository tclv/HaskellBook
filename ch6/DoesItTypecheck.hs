-- 1
data Person = Person Bool deriving Show --Did not derive show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving (Show, Eq) -- Did not derive Eq

settleDown :: Mood -> Mood -- No type signature
settleDown x = if x == Woot then Blah else x

-- 3
-- a) Values of the type Mood due to the equality check with `Woot`.
-- b) Type error 
-- c) Type error that Mood is not an instance of Ord typeclass.

-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool" -- This is; curried data constructor

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"
