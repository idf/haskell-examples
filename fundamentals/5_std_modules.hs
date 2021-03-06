import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
{-
# Modules

Import must in the head. Unable to import in the middle
## import
import Data.List Data.Set
## import
import Data.List (nub, sort)
## import except
import Data.List hiding (nub)
## QUALIFIED import, call func with fully-qualified name
import qualified Data.Map
import qualified Data.Map as M

STL browser:
https://www.haskell.org/hoogle/
-}

{-
# Data.List
## common funcs:
* intersperse: interleave
> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"

* intercalate: join
> intercalate " " ["hey","there","guys"]
"hey there guys"

* transpose: mat.T
> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]

* foldl', foldl1': non-lazy. avoid thunk stackoverflow

* concat: extend, flatten
> concat [[3,4,5],[2,3,4],[2,1,1]]
[3,4,5,2,3,4,2,1,1]

* and: all true
> and $ map (>4) [5,6,7,8]
True
> and . map (>4) $ [5,6,7,8]
True

* or: any false

* all: and . map
> all (>4) [6,9,10]
True

* any: or . map

* iterate: iteratively apply to the accumulated
> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]

* splitAt: split a list at index, (A[:i], A[:i])
> splitAt 5 "helloworld"
("hello","world")

* takeWhile: replace filter for infinite list
* dropWhile: similar to takeWhile, keep the reset
* span: combination of takewhile and dropWhile
> span (/=' ') "This is a sentence"
("This"," is a sentence")
* break: similar to span
> break (==' ') "This is a sentence"
("This"," is a sentence")

* sort
* group: group adjancent cells if equal
> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,
   2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
-- notice @ for name binding

* inits, tails: iteratively init, tail
> inits "w00t"
["","w","w0","w00","w00t"]
> tails "w00t"
["w00t","00t","0t","t",""]
> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

* isInfixOf: search
* isPrefixOf, isSuffixOf
* elem, notElem

* partition, iterate to collect to two buckets
> partition (>3) [1,3,5,6,3,2,1,0,3,7]
([5,6,7],[1,3,3,2,1,0,3])

* find: return the element.
-- Maybe is Just something or Nothing, similar to java.util.Optional<T>
find :: (a -> Bool) -> [a] -> Maybe a
> find (>4) [1,2,3,4,5,6]
Just 5
> find (>9) [1,2,3,4,5,6]
Nothing

* elemIndex: return the index
elemIndex :: Eq a => a -> [a] -> Maybe Int
* elemIndices: return all the indices
elemIndices :: Eq a => a -> [a] -> [Int]

* findIndex
> findIndex (==4) [5,3,2,1,6,4]
Just 5
* findIndices
> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
[0,6,10,14]
* zip3
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
* zipwith3
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

* lines: break string using \n
* unlines: join list of string into string with \n
* words
* unwords

* nub: de-duplicate, essential part of something, O(n^2)
> nub "Lots of words and stuff"
"Lots fwrdanu"

* delete: delete first occurrence

* (\\): list diff, infix
* union, intersect

* insert: iterate and insert
> insert 3 [1,2,4,3,2,1]
[1,2,3,4,3,2,1]

* genericLength, generic- Take, genericDrop, genericSplitAt, genericIndex and genericReplicate
length :: [a] -> Int
genericLength : (Num a) => [b] -> a

* nubBy, deleteBy, unionBy, intersectBy and groupBy
takes equality func
group is groupBy (==)
* sortBy, insertBy, maximumBy and minimumBy

* on
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

## Examples of
* take
* groupBy
* on
* sortBy
-}
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- | group by signs
groupBySign :: [[Double]]
groupBySign = let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1]
                in groupBy (\x y -> (x > 0) == (y > 0)) values

-- | doing (==) `on` (> 0)
groupBySign' :: [[Double]]
groupBySign' = let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1]
                 in groupBy ((==) `on` (>0)) values

-- | compare using key
sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy (compare `on` length) xs

{-
# Data.Char
## predicate :: Char -> Bool
isControl, isSpace, isLower, isUpper, isAlpha, isUpper, isAlpha, isAlphaNum,
isPrint, isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber,
isPunctuation, isSymbox, isSeparator, isAscii, isLatin1, isAsciiUpper,
isAsciiLower

## GeneralCategory
> map generalCategory " \t\nA9?|"
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

## Converter
* toUpper, toLower
* toTitle: convert to title case
* digitToInt, intToDigit: "int" <-> int, in Hex
* ord, chr: char <-> int

## Examples
-}

-- | predicate
checkAlphaNum = all isAlphaNum "bobby283"

-- | Caesar cipher, encode by shift
encode :: Int -> String -> String
encode offset msg = map (chr . (+ offset) . ord) msg

-- | Caesar decipher
decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg


{-
# Data.Map
Internal implementation: tree

## Common functions
* fromList
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
Ordable since using tree in Map.

* insert
> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
fromList [(3,100),(4,200),(5,600)]

* null: check whether empty
* size :: Map.Map k a -> Int
* member :: Ord k => k -> Map.Map k a -> Bool
* map, filter works on vals

* toList
* fromListWith: handle dup
> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
* insertWith: handle dup
> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
fromList [(3,104),(5,103),(6,339)]

## Examples
-}

phoneBook =
    [("betty","555-2938"),
     ("bonnie","452-2928")
    ]

-- snd: second component of the pair
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- | safer than above using Maybe typeclass
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- | implementation of 'fromList'
fromList' :: Ord k => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

-- | use func to handle dup
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\a b -> a ++ ", " ++ b) xs

-- | Map.Map k [a]
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs


{-
# Data.Set
Internal implementation: tree, thus sorted

## Common functions
* fromList
Set.fromList :: Ord a => [a] -> Set.Set a

* intersection
* difference
* union

* null, size, member, empty, singleton, insert and delete
* isSubsetOf, isProperSubsetOf

* map, filter

## Examples
-}

{-
## nub
nub :: Eq a => [a] -> [a]
nub O(n^2), since only Eq not Ord is required in List.
Deduplicate. The nub function removes duplicate elements from a list. In particular,
it keeps only the first occurrence of each element.
-}
-- | replace nub, faster, but order is not preserved
setNub xs = Set.toList $ Set.fromList xs
