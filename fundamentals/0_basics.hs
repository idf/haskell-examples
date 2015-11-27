{-
# List, varaible length, possible infinite length, homogenous type

* Singleton list: [1] allowed
* The syntax comes from the math set comprehension.
-}
-- | List Comprehension example with predicate
listComprehension = [ 2*x | x <- [50..100], x `mod` 2 == 1]
-- | fuzzy buzzy
fuzzyBuzzy xs = [ if x < 10 then "fuzzy" else "buzzy" | x <- xs, odd x]

-- | cartesian product
dualLists = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- | length
length' xs = sum [1 | _ <- xs]
-- | string process
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- | nested list, process recursively, without flattening
-- | function without param is a definition.
xxs = [[1,3,5,2,3,1,2,4,5], [1,2,3,4,5,6,7,8,9], [1,2,4,2,1,6,3,1,3,2,3,6]]
removeOddNestedLists xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

{-
# Tuple: fixed length, finite length, heterogenous type

* Tuples of different lengths/ heterogenous type will form different types
* Non-singleton tuple: (1, ) not allowed
* Tuples are types but they are dependent on their length as well as the types of thier components
-}
-- | longer list simply gets cut off to match the length of the shorter one.
zipSample = zip [1..] ["apple", "orange", "cherry", "mango"]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
