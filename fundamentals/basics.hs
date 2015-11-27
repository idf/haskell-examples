-- | List Comprehension example with predicate
listComprehension = [ 2*x | x <- [50..100], x `mod` 2 == 1]
-- | fuzzy buzzy
fuzzyBuzzy xs = [ if x < 10 then "fuzzy" else "buzzy" | x <- xs, odd x]
