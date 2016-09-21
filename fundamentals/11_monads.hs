{-
Monads
-}

-- Functor => Applicative Functor => Monads
fmap :: (Functor f) => (a -> b) -> f a -> f b
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- notice the differences among the functions intaken.


{-
# Monad Class

every monad is an applicative functor
-}

class Monad' m where
    return :: a -> m a  -- boxing
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind, then

class Monad m where
    return :: a -> m a  -- like pure in ap functor

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b  -- default
    x >> y = x >>= \_ -> y

    fail :: String -> m a  -- default
    fail msg = error msg

instance Monad Maybe where 
    return x = Just x 
    Nothing >>= f = Nothing 
    Just x >>= f = f x 
    fail _ = Nothing



{-
# Walk the line
-}

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

{-
>>=
-}
ex1 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

{-
>>

passing some value to a function that ignores its parameter and always just returns some predetermined 
value would always result in that predetermined value. With monads how- ever, their context and 
meaning has to be considered as well.

ghci> Nothing >> Just 3
Nothing
ghci> Just 3 >> Just 4
Just 4
ghci> Just 3 >> Nothing
Nothing
-}
ex2 = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1



{-
# do notation
-}

-- assign 3 to x, "!" to y
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

{-
do - imperative code?

do expressions are written line by line, they may look like imperative code to some people. 
But the thing is, they're just sequential, as each value in each line relies on the result 
of the previous ones, along with their contexts

More like nested-functions
-}
routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second


routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing  -- sequence the monadic value but we ignore its result
    second <- landRight 2 first
    landLeft 1 second

-- pattern matching on the LHS. If fail, fail msg
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

{-
# List monad

non-deterministic values
-}
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)  -- flatten
    fail _ = []

{-
> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

list comprehensions are just syntactic sugar for using lists as monads. In the end, 
list comprehensions and lists in do notation translate to using >>= to do computations 
that feature non-determinism.
-}
listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

{-
MonadPlus type class is for monads that can also act as monoids
-}

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a


instance MonadPlus [] where
    mzero = []
    mplus = (++)

{-
> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
[7,17,27,37,47]
-}
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

{-
# Monad law

Left identity
Right identity
Associativity
-}

{-
Left identity

return x >>= f \equiv f x
-}

li1 = return 3 >>= (\x -> Just (x+100000))
li2 = (\x -> Just (x+100000)) 3

{-
Right identity

m >>= return \equiv m
-}

ri = Just "move on up" >>= (\x -> return x)

{-
Associativity

(m >>= f) >>= g \equiv m >>= (\x -> f x >>= g)
-}

assoc1 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
assoc2 = return (0,0) >>= (\x ->
    landRight 2 x >>= (\y ->
    landLeft 2 y >>= (\z ->
    landRight 2 z)))

-- function composition at monad level
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

