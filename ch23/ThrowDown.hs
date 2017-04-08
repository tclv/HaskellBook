import System.Random

import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import Control.Applicative (liftA2, liftA3)

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = 
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes =
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
   in (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie' rollDie' rollDie'


-- Repeats a single value
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- What you actually want
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollDieT :: State StdGen (Sum Int, [Die])
rollDieT = liftA2 (,) Sum (pure . intToDie) <$> state (randomR (1, 6))

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go (Sum 0, []) g
  where
    go :: (Sum Int, [Die]) -> StdGen -> (Int, [Die])
    go s@(Sum sum, dies) gen
      | sum >= n = (length dies, dies)
      | otherwise       =
        let (die, nextGen) = runState rollDieT gen
         in go (s <> die) nextGen
      
rollsToGetN' :: Int -> IO (Int, [Die])
rollsToGetN' n = (rollsToGetN n) . mkStdGen <$> randomIO
