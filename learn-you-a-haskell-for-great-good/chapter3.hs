----------------------------- TYPES -----------------------------

{-
Things to remember about types in Haskell
+ Haskell has a static type system. The type of every expression is known at compile time, which leads to safer code.
+ Unlike Java or Pascal, Haskell has type inference. If we write a number, we don't have to tell Haskell it's a number.
+ We can use the :t command in GHCI to get the type of expressions.

Try the following command in ghci
ghci> :t 'a'  
'a' :: Char
ghci> :t "HELLO"
"HELLO" :: [Char]
ghci> :t (True, 3)
(True, 3) :: Num b => (Bool, b)
ghci> :t 3 == 4 
3 == 4 :: Bool

Tip: the :: symbol is read as "has type of"
-}

-- Int is bounded integer, which means that it has a minimum and a maximum value
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer is unbounded integer, so it can be used to represent really really big numbers
factorial :: Integer -> Integer 
factorial n = product [1..n]

-- Float is a real floating point with single precision
circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double is a real floating point with double the precision
circumference' :: Double -> Double 
circumference' r = 2 * pi * r

-- a is a type variable. That means that a can be of any type. Functions that have type variables are called polymorphic functions
head' :: [a] -> a
head' xs = head xs


----------------------------- TYPECLASSES -----------------------------

{-
Things to remember about typeclass:
+ A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes.
-}


{-
ghci> :t (==)  
(==) :: (Eq a) => a -> a -> Bool 

Everything before the => symbol is called a class constraint. We can read the above type declaration like this: the equality function takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class (this was the class constraint)
-}


{-
Some basic typeclasses:
+ Eq is used for types that support equality testing
+ Ord is for types that have an ordering
+ Members of Show can be presented as strings
+ Read is sort of the opposite typeclass of Show. The read function takes a string and returns a type which is a member of Read
+ Enum members are sequentially ordered types — they can be enumerated
+ Bounded members have an upper and a lower bound
+ Num is a numeric typeclass. Its members have the property of being able to act like numbers
+ Integral is also a numeric typeclass but it only includes integral (whole) numbers
+ Floating includes only floating point numbers, so Float and Double
-}