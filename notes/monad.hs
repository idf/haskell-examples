{-
# Functor, Applicative, Monad
Functor: (a -> b)
Applicative: f (a -> b)
Monad: (a -> m b)
-}
($) :: (a -> b) -> a -> b
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a ->f b
(>>=) :: Monad m => (a -> m b) -> m a -> m b

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Applicative & Lift
lift2 f x1s x2s = pure f <*> x1s <*> x2s

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- what func takes as argument is capured in the do-statement

{-
# State
-}
data ST s a = S (s -> (a, s))

instance Monad (ST s) where
  return x = S $ \s -> (x,s)
  st >>= f = S $ \s -> let(x,s') = apply st s
                       in apply (f x) s'

apply :: ST s a -> s -> (a, s)
apply (S f) s = f s

{-
# Monad Transformer
-}
