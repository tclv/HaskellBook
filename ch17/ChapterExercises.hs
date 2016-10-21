module ChapterExercises where

-- 1.

pure  :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

-- 2.

pure  :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

-- 3.

pure  :: Monoid a => b -> (a, b)
(<*>) :: Monoid c => (c, a -> b) -> (c, a) -> (c, b)

-- 4.

pure :: a -> (->) e a
