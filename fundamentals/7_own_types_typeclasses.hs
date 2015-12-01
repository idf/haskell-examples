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
aCar = Car {company="Ford", model="Mustang", year=1967}

{-
# Type parameters

Value constructor -> type constructor
e.g. "a" as the type parameter
* Maybe a
* [a]

-}

-- | Implementation of "Maybe"
-- a is a type parameter, thus Maybe' generates a type
-- No value can have a type of Maybe, because that's not a type per se, it's a type constructor.
data Maybe' a = Nothing | Just a
