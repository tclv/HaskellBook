import Control.Monad
import Data.Char
import System.Exit (exitSuccess)


palindrome :: IO ()
palindrome = forever $ do
  line1 <- cleanInput <$> getLine
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!" >> exitSuccess

cleanInput :: String -> String
cleanInput = filter isLetter . map toLower

main :: IO ()
main = palindrome
