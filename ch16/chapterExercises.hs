--1. No (need * -> *)
--2. Yes
--3. Yes
--4. 
newtype Mu f = InF { outF :: f (Mu f) }

{-instance Functor Mu where-}
  {-fmap g (Mu f (Mu f)) = undefined-}


