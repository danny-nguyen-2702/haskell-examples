-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

-- the Bounded type class provides two variables: minBound and maxBound. These two variables have the minimum and maximum value of a type
-- for example: minBound :: Int returns 9223372036854775807

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

-- the Int type store both negative and positive values, but the Word type only have positive values. The Word type is like the uint type of other programming languages


-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

-- the Enum type class defines functions to enumerate values. For example, the succ function return the next value of a value, and the pred return the previous valud of a value
-- This type class is the one that allows us to create ranges of values like [3..] and ['a'..'h']

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.
f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Bounded a, Eq a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

-- the fromInteger function changes a Integer value to a Numeric value