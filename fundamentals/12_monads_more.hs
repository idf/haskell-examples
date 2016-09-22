{-
Monads More
-}

{-
# Writer
Writer monad is for values that have another value attached that acts as a sort of log value.
-}

-- without Writer, we need to do
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- with Writer
newtype Writer w a = Writer { runWriter :: (a, w) }  -- why w a reversed?

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')


-- do notation with Writer
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

{-
Creates a Writer value that presents the dummy value() as its result but has a desired monoid value attached.
-}
multWithLog' :: Writer [String] Int
multWithLog' = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]  -- desired monoid
    return (a*b)


-- Adding logging to programs
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)


{-
Difference list

In the previous section, list appending takes much time and space.

To solve, use functions chaining for list efficient appending: \xs -> [1,2,3] ++ xs
f `append` g = \xs -> f (g xs)
-}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))


{-
# Functions as monad
Functions are: Functor, Applicative Functor, Monad.

a function can also be considered a value with a context.  The context for functions is that that value 
is not present yet and that we have to apply that function to something in order to get its result value.

In Control.Monad.Instances 
-}

instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w


addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

-- the function monad is also called the reader monad. All the functions read from a common source.
addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a+b



{-
# State
-}

-- take a state, and return a result and a new state.
-- s -> (a, s) is a function for stateful computation
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

-- stack
pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

{-
> runState stackStuff [9,0,2,1,0]
((),[8,3,0,2,1,0])

Need an initial feed
-}
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do 
            push 3
            push 8

-- random
import System.Random
randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random


{-
threeCoins is now a stateful computations and after taking an initial random generator, it passes 
it to the  first randomSt, which produces a number and a new generator, which gets passed to the next one and so on. 

do notation: auto feed to the next line.

Need an initial feed
-}
threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

r = runState threeCoins (mkStdGen 33)  --  ((True,False,True),680029187 2103410263)

{-
# Error

Either e a  -- e for error
-}
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)

{-
> Right 3 >>= \x -> return (x + 100) :: Either String Int
Right 103
-}



{-
# Monadic functions

Every monad is an applicative functor and every applicative functor is a functor.
-}

-- liftM and fmap do the similar thing
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

fmap :: (Functor f) => (a -> b) -> f a -> f b
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- ap function is basically <*>, only it has a Monad constraint instead of an Applicative one.
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

-- join, flatten the nested monads
join :: (Monad m) => m (m a) -> m a

-- filter
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

-- fold
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a