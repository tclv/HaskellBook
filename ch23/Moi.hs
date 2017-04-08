

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s
                                in (f a, s')

instance Applicative (Moi s) where
  (Moi f) <*> (Moi g) = Moi $ \s -> let (h, s') = f s
                                        (a, s'') = g s' 
                                     in (h a, s'')

  pure a = Moi $ \ s -> (a, s)

instance Monad (Moi s) where
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                  Moi h   = g a
                               in h s'

  return = pure
