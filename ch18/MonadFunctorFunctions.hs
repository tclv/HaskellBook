import Control.Monad

j :: Monad m => m (m a) -> m a
j = (>>= id) -- equivalent to join
-- j = join -- definition

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = (>>= return . f) -- equivalent fmap

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' f x = x >>= (\x -> f >>= (\f -> (return . f) x))
-- equivalent to ap/<*>

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = l1 f x `ap` y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr ap' (pure []) $ (l1 . l1) (:) (l1 f xs)

-- Does not require Monad
mehA :: Applicative f => [a] -> (a -> f b) -> f [b]
mehA xs f = foldr (<*>) (pure []) $ (fmap . fmap) (:) (fmap f xs)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
