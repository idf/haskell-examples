{-
Functors, Applicative Functors and Monoids
-}

{-
# Functors redux
-}

class Functor' f where
    fmap :: (a -> b) -> f a -> f b

{-
IO is a functor
-}
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

{-
Function is a functor.
fmap over two functions is just composition
-}
instance Functor ((->) r) where
    fmap f g = (\x -> f . g $ x)

instance Functor ((->) r) where
    fmap = (.)

{-
> fmap (*3) (+100) 1
303
-}
fmap (*3) (+100)

{-
Lifting a value is when you take a value and put it into an object like a functor. 

If you lift a function into an Applicative Functor then you can make it work on values that are also in that functor.
fmap :: (a -> b) -> f a -> f b  is equivalent to
fmap :: (a -> b) -> (f a -> f b)

fmap as either:
1. a function that takes a function and a functor and then maps that function over the functor, 
2. a function that takes a function and lifts that function so that it operates on functors. 
-}
times2 :: (Num a, Functor f) => f a -> f a
times2 = fmap (*2)

{-
Functor Law 1: fmap id = id.
Functor Law 2: fmap (f . g) = fmap f . fmap g 
-}

{-
# Applicative functors

mapping "multi-parameter" functions over functors, we get functors that contain functions inside them
f (a -> b)

how fmap is defined for applicatives:
fmap l x = pure l <*> x

-}
af = fmap (*) [1,2,3,4]
concrete = fmap (\f -> f 9) af

class (Functor f) => Applicative' f where
    pure :: a -> f a  -- warp value in a functor; pure context
    (<*>) :: f (a -> b) -> f a -> f b  -- .ap operation

{-
 <$> is just an infix alias for fmap

Operations in the same level/context of functors. Computation context.
f <$> x <*> y <*> z. If the parameters weren't applicative functors but normal values, we'd write f x y z.
same as: pure f <*> x <*> y <*> z
-}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
func <$> x = fmap func x  -- infix alias for fmap, fmap for function is composition

{-
List is ap functor
-}
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

{-
IO is ap functor
-}
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

{-
Function is ap functor
Functions as boxes that contain their eventual results
(r -> a -> b) -> (r -> a) -> (r -> b)
-}
instance Applicative ((->) r) where
    pure x = (\_ -> x)  -- to unbox, apply any param
    f <*> g = \x -> f x (g x)


{-
(+) <$> (+3) <*> (*100) $ 5 is 508, which is the same as
pure (+) <*> (+3) <*> (*100) $ 5

pure (+) x = (+)  -- inbox and then unbox
-}
func :: (Num a) => a -> a
func = (+) <$> (+3) <*> (*100)
