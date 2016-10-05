{-
Functions
-}

{-
# Pattern Matching
Pattern matching are not just for function params, but also in DataList; `case`; value constructor;
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

-- | @ pattern: xs@(x0:x1:x2s), syntatic sugar

{-
# Guards `|`
it is not `case`
-}
a `cmp` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

{-
# WHERE statement
-}
-- | "where" for shorthand local variable notations
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- | "where" for shorthand local func notations, like mapper
-- following math convvetion
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight/height^2

{-
# Let
-}
-- | leet .. in ..: let <bindings> in <expression>
letVar = let a = 100; b = 200; c = 300 in a*b*c
letFunc = [let square x = x * x in (square 2, square 3, square 5)]
-- | let for list comprehension
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs, let bmi w h = w / h ^ 2]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] -- without params

{-
# Case
case <expr> of <pat> -> <ret>
               <pat> -> <ret>

* Also uses pattern matching
* Compared to function pattern matching, case can be used in anywhere
-}
head'' :: [a] -> a
head'' xs = case xs of [] -> error "empty"
                       (x:_) -> x


range :: Int -> Int -> [Int]
range i j
  | i < j = i : range (i+1) j
  | otherwise = []
