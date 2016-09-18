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


{-
With ordinary functors, we can just map functions over one functor. 
But with applicative functors, we can apply a function between several functors
-}
liftA  :: Applicative t => (a -> b) -> t a -> t b

liftA2 :: Applicative t => (a1 -> a2 -> b) -> t a1 -> t a2 -> t b
liftA2 f a1 a2 = f <$> a1 <*> a2

liftA3 :: Applicative t 
       => (a1 -> a2 -> a3 -> b) 
       -> t a1 
       -> t a2
       -> t a3
       -> t b


{-
> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
> and $ map (\f -> f 7) [(>4),(<10),odd]
True

> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]

> sequenceA [(>4),(<10),odd] 7
[True,True,True]
-}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

{-
Applicative Functor Laws:
f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w= u <*> ( v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
-} 



{-
# The newtype keyword

data: to make our own algebraic data types.
type: to give existing types synonyms.
newtype: to make new types wrapping existing data types. 
-}

{-
take one type and wrap it in something to present it as another type.
-}
newtype ZipList a = ZipList { getZipList :: [a] }

{-
Using newtype to make type class instance
-}
newtype Pair a b = Pair { getPair :: (a,b) }


-- Functor requires its instances to be type constructors which take one parameter.
instance Functor (Pair c) where
    -- fmap :: (a -> b) -> Pair c a -> Pair c b
    fmap f (Pair (x,y)) = Pair (f x, y)

{-
On newtype laziness
newtype types can only have one possible value constructor and one field
-}



{-
# Monoids
-}
class Monoid m where
    mempty :: m  -- identity value
    mappend :: m -> m -> m  -- binary func
    mconcat :: [m] -> m  -- reduce
    mconcat = foldr mappend mempty


{-
Monoid laws:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}


-- Lists are Monoid
instance Monoid [a] where
    mempty = []
    mappend = (++)