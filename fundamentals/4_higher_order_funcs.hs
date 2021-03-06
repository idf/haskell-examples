{-
Higher order functions

Functions that takes func params or return func
-}

{-
# Currying

Every function in Haskell officially only takes one parameter.
-}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- | to curry infix func: 1. parethesize and 2. supply RIGHT parameter
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)  -- INFIX

-- | check upper case
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])  -- easier to use infix
-- `op` makes prefix operator become infix

{-
# Higher-orderism
-}
applyTwice :: (a -> a) -> a -> a  -- right associative
applyTwice f x = f (f x)
applyTwice' f x = f $ f x

-- | implementation of "zipWith" func
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

-- | flip param, implementation of "flip", FILP PARAMS
-- flip is frequently used with currying to swap the right next func's left & right params
-- e.g. zipWith (flip div) [2,2..] [10,8,6,4,2]
-- (a -> b -> c) -> b -> a -> c is same as (a -> b -> c) -> (b -> a -> c) due to right associative
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- | alternatively use lambda, equivalent
-- f is a func taking two params, then flip f = a lambda func
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

{-
# Maps and Filters map filter
-}
-- | implementation of "map"
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- | implementation of "filter"
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- | filter version of quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let l = quicksort (filter (<=x) xs)
        r = quicksort (filter (>x) xs)
    in  l ++ [x] ++ r

-- | laziness
-- takeWhile: take something while true until stops (false)
sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  -- brackets for currying

-- | Collatz sequences
-- Half Or Triple Plus One
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int  -- Int for length
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

{-
# Lambdas \

* \<params> -> <body>
* anonymous functions used once
* Lambdas are expressions that return a func
* Normally surrounded by parenthesis
-}
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- | since defined once, only single pattern matching
sumPair = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

{-
# Folds foldl foldr

Reduce
* Fold from left:
    foldl <func :: acc -> cur -> a> <acc> <list>,
  where func has acc on the LEFT
* Fold from right
    foldr <func :: cur -> acc -> a> <acc> <list>,
  where func has acc on the RIGHT
* Only foldr rather than foldl works on infinite list

Implicit starting value
* foldl1 <func> <list>
* foldr1 <func> <list>

Scan
* scanl, scanr, scanl1, scanr1
scanl1 is a variant of scanl without starting value argument
-}
-- | usage of fold left, which starts from the left side.
sumFold :: (Num a) => [a] -> a
sumFold xs = foldl (\acc x -> acc + x) 0 xs

sumFoldCurry :: (Num a) => [a] -> a
sumFoldCurry = foldl (+) 0

elemFold :: (Eq a) => a -> [a] -> Bool
elemFold y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- | when building new lists from a list, normally use foldr
-- map using fold, accumulate to a list
mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- | only foldr works on infinite list: e.g. mapFoldr (+3) [1, 3..]
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

-- | implicit starting value
-- If the function doesn't make sense when given an empty list, then use foldl1
headFold :: [a] -> a
headFold = foldr1 (\x _ -> x)  -- from right, take the cur
lastFold :: [a] -> a
lastFold = foldl1 (\_ x -> x)  -- from left, take the cur

-- currying
reverseFold :: [a] -> [a]
reverseFold = foldl (\acc x -> x : acc) []

-- scanl & scanr will display the intermediate result
-- thus like dp
-- | laregest until now
maxDP = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]

-- | number of elements to take for the sum of the roots of all natural numbers to exceed 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{-
# Function Application `$`

($) :: (a -> b) -> a -> b  -- thus, without brackets, $ is infix op
f $ x = f x

* Normally left-associative
* Function application with a space is LEFT-associative (so f a b c is the same
as ((f a) b) c)), function application with $ is RIGHT-associative, since the
left's precedence is reduced.
* $ has the lowest precedence
* Usage: 1) get rid of parentheseses 2) apply function

-}

-- | get rid of parentheseses
sumParenthesis = sum (map sqrt [1..130])
sumFuncApply = sum $ map sqrt [1..130]

-- | pass function application `$` as func
funcApply = map ($ 3) [(4+), (10*), (^2), sqrt]
-- ($ 3) is different from (($) 3) for param position.

{-
# Function Composition `.`

It is like mathematical funciton composition,
and it is independent of function application `$`
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

* Function composition is RIGHT associative, rather than normal left associative
* Only composes funcs that takes ONE param, thus currying is needed for
multi-param func

-}

-- | Replace lambda expressions
negAllLambda = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
negAllLambdaApplication = map (\x -> negate $ abs x) [5,-3,-6,7,-3,2,-19,24]
negAllComp = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- | Remove parentheseses
funcParen = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
funcComp = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
-- zipWith max [1,2,3,4,5] [4,5,6,7,8] works, but for the func composition above,
-- it must have `$` to curry zipWith taking one param

-- | Point free style (i.e. f(x) without param x)
funcNormal x = ceiling (negate (tan (cos (max 50 x))))
funcPointFree = ceiling . negate . tan . cos . max 50

{-
Function application vs. function composition
k = a . b . c $ value
is preferred over
k = a $ b $ c $ value

compose with (.) FIRST and then only at the END use ($) to append a value to
evaluate the lot
-}

-- | Let binding vs. func composition
oddSquareSum1 :: Integer
oddSquareSum1 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- | this function style is preferred
oddSquareSum2 :: Integer
oddSquareSum2 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- | for more readability
oddSquareSum3 :: Integer
oddSquareSum3 =
    let oddSquares = filter odd . map (^2) $ [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

-- alternatively, oddSquares = filter odd $ map (^2) [1..]
