import qualified Data.Map as Map
{-
# Own types and typeclasses
-}

{-
# Algebraic data types
-}

-- | Value constructors are actually functions that ultimately return a value of a data type.
-- Shape is a type, Circle isn't.
data Shape1 = Circle1 Float Float Float | Rectangle1 Float Float Float Float
           deriving (Show) -- show

-- | Pattern matching for constructors
surface1 :: Shape1 -> Float
surface1 (Circle1 _ _ r) = pi * r ^ 2
surface1 (Rectangle1 x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- | Currying for value constructors
circles1 = map (Circle1 10 20) [4,5,6,6]

-- | used the same name for the data type and the value constructor, if only one constructor
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- | nested pattern matching
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
To export all the value constructors for a given type, just write ..
module Shapes
( Point(..)
, Shape(..)
) where

If opt not to export any value constructors for Shape by just writing Shape in the export statement.
That way, someone importing our module could only make shapes by using the auxilliary functions.
-}

{-
# Record Syntax

creates functions that lookup fields in the data type.
-}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- | dict constructor
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

aCar = Car "Ford" "Mustang" 1967
aCarDict = Car {company="Ford", model="Mustang", year=1967}

showCar :: Car -> String
showCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-
# Type parameters
## Type constructor
Value constructor -> type constructor
data <Type> <param> = ... e.g. "a" as the type parameter. Similar to Java
generics
* Maybe a
* [a]

A list of stuff is a list of stuff and it doesn't matter what the type of that
stuff is, it can still work.

Polymorphic type: Nothing is polymorphic type

## Typeclass constraints in data
Strong convention in Haskell to never add typeclass constraints in data
declarations, since have to put them into the function type declarations
either way.
-}

-- | Implementation of "Maybe"
-- a is a type parameter, thus Maybe' generates a type, e.g. Maybe Int
-- No value can have a type of Maybe, because that's not a type per se, it's a type constructor.
data Maybe' a = Nothing' | Just' a
-- | Just Int, the Int type is inferred
aMaybe = Just 0

-- | Car String String int, although no practical usage.
data Car' a b c = Car' { company' :: a
                       , model' :: b
                       , year' :: c
                       } deriving (Show)

aCar' = Car' "Ford" "Mustang" 1967

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-
# Derived Instance
## Typeclass
"typeclass" is like a interface, "type" implemnts the interface.
e.g. the Int type is an instance of the Eq typeclass.
deriving (<typeclass>)

Type vs. Typeclass
If treat "type" as value, "typeclass" becomes type function, takes type value and returns a new type; type constructor.

Ord
* First comes in value constructor is smaller
* If equal, nested compare

Enum
* successor, predecessor: succ, pred

Bounded
* head smallest, tail largest;
* minBound, maxBound
-}

data Person' = Person' { firstName' :: String
                       , lastName' :: String
                       , age' :: Int
                       } deriving (Eq, Show, Read)

deserializePerson = read "Person' {firstName' =\"Michael\", lastName' =\"Diamond\", age' = 43}" :: Person'

-- | nullary constructor
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | ellipsis
partialDays = [Thursday .. Sunday]

-- | range
entireDays = [minBound .. maxBound] :: [Day]


{-
# Type synonyms

Different name binding:
type String = [Char]

Type constructors vs. Value constructors
unable to do: TypeConstructor [(1,2),(4,5),(7,9)]
able to do: [(1,2),(3,5),(8,9)] :: TypeConstructor Int Int
-}

type PhoneBook = [(String,String)]

-- | Type constructor, parameterized type synonyms
type AssocList k v = [(k,v)]

-- | Type constructor currying, since type constructor is a func
type IntMap v = Map.Map Int v
type IntMap' = Map.Map Int

-- | Implementation of "Either", value constructor
-- errors use the Left value constructor while results use Right
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)


-- | locker example
data LockerState = Taken | Free deriving (Show, Eq) -- not Enum, but like enumerate
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  -- returns Either
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of  -- Map.lookup returns Maybe
         Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
         Just (state, code) -> if state /= Taken then
                                   Right code
                               else
                                   Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

{-
Recursive Data Structures
-}
