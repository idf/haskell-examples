{-
Recursion

no "while" loops or "for" loops in Haskell
-}
-- | "maximum"
-- `a` is a type param
maxLst :: (Ord a) => [a] -> a
maxLst [] = error "empty list"
maxLst [x] = x
maxLst (x:xs) | x > maxTail = x
              | otherwise = maxTail
              where maxTail = maxLst xs

maxLst' :: (Ord a) => [a] -> a
maxLst' [] = error "empty"
maxLst' [x] = x
maxLst' (x:xs) = max x (maxLst' xs)

-- | implementation of "take", [:n]
-- `,` is like and
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n<=0 =[]
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- | implementation of "reverse"
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- | infinite list, generator
repeat' :: a -> [a]
repeat' x = x:repeat' x


-- | "zip", process two lists
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []  -- stops when any of the list is empty
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- | implementation of "elem"
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a==x =True
    | otherwise = a `elem'` xs

-- | quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let left = quicksort [a | a <- xs, a <= x]
        right = quicksort [a | a <- xs, a > x]
    in  left ++ [x] ++ right
-- can be rewritten using `where`
