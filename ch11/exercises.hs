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

-- Generate all programmer possibilities 

data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindJustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgrammingLanguage =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
           deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill 
                      , Mac
                      , Windows 
                      ]
allLanguages :: [ProgrammingLanguage] 
allLanguages = [Haskell, Agda, Idris, PureScript]


allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]
