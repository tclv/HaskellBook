

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s
                                    in (f a, s')

instance Applicative (State s) where
  (State f) <*> (State g) = State $ \s -> let (h, s') = f s
                                              (a, s'') = g s' 
                                           in (h a, s'')

  pure a = State $ \ s -> (a, s)

instance Monad (State s) where
  (State f) >>= g = State $ \s -> let (a, s') = f s
                                      State h   = g a
                                   in h s'

  return = pure


get :: State s s
get = State $ \ s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec s = snd . runState s

eval :: State s a -> s -> a
eval s = fst . runState s

modify :: (s -> s) -> State s ()
modify f = f <$> get >>= put
