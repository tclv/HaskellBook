-- Given
data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)


-- 1

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)  -- Need to add explicit 
                                          -- data constructors
                                          -- arguments

-- 2

truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)  -- compiles

-- 3

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'                  -- compiles

-- 4

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'  -- Does not compile need to be member
                            -- of Ord typeclass (fixed above)
