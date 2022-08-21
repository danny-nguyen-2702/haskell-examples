import Data.List (intercalate)

-- Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a) => a -> a

mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum x = x + mySum (x-1)



-- Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a

myMul :: (Eq a, Num a) => a -> a -> a
myMul 0 y = 0
myMul x y = y + myMul (x-1) y



-- Our dividedBy function wasn’t quite ideal. For one thing. It was a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less. Using the pre-existing div function we can see how negative numbers should be handled

dividedByNegative :: Integral a => a -> a -> (a, a)
dividedByNegative num denom = go (abs num) (abs denom) 0
    where   go n d count
                | n < d = (count, n)
                | otherwise = go (n - d) d (changeCount count)
            changeCount = if num * denom >= 0 then (+1) else subtract 1



-- The next issue is how to handle zero. Zero is undefined for division in math, so we ought to use a datatype that lets us say there was no sensible result when the user divides by zero

data DividedResult = Result Integer | DividedByZero deriving Show

dividedByZero :: Integral a => a -> a -> (DividedResult, a)
dividedByZero num denom 
    | denom == 0 = (DividedByZero, 0)
    | otherwise = go (abs num) (abs denom) 0
    where   go n d count
                | n < d = (Result count, n)
                | otherwise = go (n - d) d (changeCount count)
            changeCount = if num * denom >= 0 then (+1) else subtract 1



-- The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise. The function is recursive.

mc91 :: (Ord a, Num a) => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11



-- Numbers into words

digitToWord :: Int -> String
digitToWord n = 
    case n of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        _ -> "none"

digits :: Int -> [Int]
digits n 
    | n < 10 = [n]
    | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
