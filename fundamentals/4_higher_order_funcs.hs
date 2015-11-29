{-
Higher order functions
Functions that takes func params or return func
-}

{-
Currying: Every function in Haskell officially only takes one parameter.
-}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- | to curry infix func: 1. parethesize. 2. supply one parameter
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- | check upper case
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])  -- easier to use infix

{-
Higher-orderism
-}
applyTwice :: (a -> a) -> a -> a  -- right associative
applyTwice f x = f (f x)

-- | implementation of "zipWith"
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

-- | flip param
-- flip is frequently used with currying to swap the right next func's params
-- e.g. zipWith (flip' div) [2,2..] [10,8,6,4,2]
-- (a -> b -> c) -> b -> a -> c is same as (a -> b -> c) -> (b -> a -> c) due to right associative
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


{-
Maps and filters
-}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
