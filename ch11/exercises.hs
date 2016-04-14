data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = 
     Garden Gardener FlowerType
     deriving Show
-- Cant distribute over tabes as this is simply (a * b) 

     
