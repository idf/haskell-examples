{-
Types and typeclasses

Haskell is statically typed with type inference

const type: capitalized
type variable: lowercase
typeclass: like interface in OOP; type constructor function taking params

TYPECLASSES:
* Eq
* Ord
* Show
* Read
* Enum
* Bounded
* Num

Polymorphic const: type coercion
Polymorphic funct: generic func
-}

-- | indifference in param types and return type in func type declarations
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

{-
type class "Eq", "Ord"
"=>" for CLASS CONSTRAINT
Special-character func "==" is infix func, "(==)" make it a normal prefix func
-}
equal' :: (Eq a) => a -> a -> Bool
equal' x y = (==) x y

-- | read :: (Read a) => String -> a
-- like eval
readLst = read "[1,2,3,4]" ++ [3]

-- | read with type annotation to resolve ambiguity when w/o context
-- | e.g. read "(3, 'a')" :: (Int, Char)
readInt :: String -> Int
readInt x = read x :: Int


-- | fromInteger :: Num a => Integer -> a
-- | fromIntegral :: (Integral a, Num b) => a -> b
