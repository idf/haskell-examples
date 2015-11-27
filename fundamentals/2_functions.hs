{-
Functions
-}

{-
Pattern Matching
-}
-- | factorial, recursive + pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- | pattern matching in list comprehension
pairSum :: (Num a) => [(a, a)] -> [a]
pairSum xs = [a+b | (a,b) <- xs]

-- | pattern matching for list
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

-- | tail recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- | pattern: xs@(x0:x1:x2s), syntatic sugar

{-
Guards
-}
a `cmp` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

{-
Where
-}
-- | "where" for shorthand local variable notations
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
-- | "where" for shorthand local func notations, like mapper
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight/height^2
