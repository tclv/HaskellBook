newtype Word' = Word' String deriving (Eq, Show)


vowels :: [Char]
vowels = ['a', 'e', 'u', 'i', 'o']

consonants :: [Char]
consonants = "bcdfghjklmnpqrstvwxyz"



notThe :: String -> Maybe String
notThe a 
  | a == "the" = Nothing
  | otherwise  = Just a

replaceThe :: String -> String
replaceThe = unwords . map replacer . words
  where
    replacer "the" = "a"
    replacer a = a

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = counter . words
  where
    counter (s1:s2:ss)
      | s1 == "the" && head s2 `elem` vowels = 1 + counter (s2:ss)
      | otherwise = 0 + counter (s2:ss)
    counter _ = 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
  where
    isVowel = (flip elem) vowels

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter isConsonant
  where
    isConsonant = (flip elem) consonants


mkWord :: String -> Maybe Word'
mkWord word 
  | countVowels word > countConsonants word = Nothing
  | otherwise = Just (Word' word)
