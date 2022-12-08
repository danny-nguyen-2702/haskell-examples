---------------------------- PATTERN MATCHING ----------------------------

-- When you call sayMe, the patterns will be checked from top to bottom and when it conforms to a pattern, the corresponding function body will be used.
sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe 6 = "Six"
sayMe 7 = "Seven"
sayMe _ = "Not between 1 and 7"

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

-- we can use pattern matching for tuples too
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x -- Notice that if you want to bind to several variables (even if one of them is just _ and doesn't actually bind at all), we have to surround them in parentheses

length' :: (Integral b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]


---------------------------- GUARDS ----------------------------

-- Whereas patterns are a way of making sure a value conforms to some form and deconstructing it, guards are a way of testing whether some property of a value (or several of them) are true or false. 
-- A guard is basically a boolean expression. If it evaluates to True, then the corresponding function body is used. If it evaluates to False, checking drops through to the next guard and so on.
-- We can combine pattern and guard together. If all the guards of a function evaluate to False (and we haven't provided an otherwise catch-all guard), evaluation falls through to the next pattern.

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

-- we can define functions in where blocks. It's a common idiom to make a function and define some helper function in its where clause
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- different between let bindings and where bindings
-- 1. let puts the bindings first and the expression that uses them later whereas where is the other way around
-- 2. let bindings are expressions themselves, where bindings are just syntactic constructs
-- 3. let bindings can't be used accross guards

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h 
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

defineLocalFunctionUsingLet = let square x = x * x in (square 5, square 3, square 2)

patternMatchingWithLet = let (_, name, _) = ("ID123", "Danny Ng", 12) in "Your name is " ++ name

severalVariablesInline = (let a = 100; b = 200; c = 300 in a*b*c, let foo="hey "; bar="there!" in foo ++ bar)

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

fatPeople :: (RealFloat a) => [(a, a)] -> [(a, a)]
fatPeople xs = [(w, h) | (w, h) <- xs, let bmi = w / h ^ 2 in bmi >= 25.0]


---------------------------- CASE EXPRESSIONS ----------------------------
{-
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
-}
head'' :: [a] -> a
head'' xs = case xs of [] -> error "Can't call head on an empty function"
                       (x:_) -> x

-- Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere. They are useful for pattern matching against something in the middle of an expression.
describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "an empty list." 
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."