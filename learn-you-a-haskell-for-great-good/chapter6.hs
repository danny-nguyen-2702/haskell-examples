--------------------------- HIGHER ORDER FUNCTIONS ---------------------------

-- Definition: Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


multTwoWith10 :: (Num a) => a -> a -> a
multTwoWith10 = multThree 10 

-- if we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out
-- Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data
-- in the example above, multThree 10 is a partially applied function



-- the compare 100 is also a partially applied function because the compare function needs 2 parameters and we just call this function with only 1 parameters, leave 1 parameters left
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  



-- Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side. That creates a function that takes one parameter and then applies it to the side that's missing an operand.
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


subtractFrom4 :: Int -> Int
subtractFrom4 = subtract 4



-- Functions can take functions as parameters
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
-- ghci> zipWith' (*) (replicate 5 2) [1..]
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]


flip' :: (x -> y -> z) -> (y -> x -> z)
flip' f x y = f y x


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]


filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs
-- ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
-- ghci> filter' even (filter' (>3) [1..10])


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' smallers ++ [x] ++ quicksort' biggers
    where smallers = filter (<=x) xs
          biggers = filter (>x) xs

-- find the largest number under 100,000 that's divisible by 3829
largestDivisible = take 1 (filter (\x -> x `mod` 3829 == 0) [100000, 99999..1])

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain x 
    | odd x = x : collatzChain (x * 3 + 1)
    | otherwise = x : collatzChain (x `div` 2)

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15
numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15


listOfFunctions :: [Integer -> Integer]
listOfFunctions = map (*) [0..] -- [(0*),(1*),(2*),(3*),(4*),(5*)..]
-- ghci> (listOfFuns !! 4) 5


--------------------------- FOLD ---------------------------

-- First let's take a look at the foldl function, also called the left fold. It folds the list up from the left side. The binary function is applied between the starting value and the head of the list. That produces a new accumulator value and the binary function is called with that value and the next element, etc.
-- if we call a fold on an empty list, the result will just be the starting value (the accumulator)

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> acc || (y == x)) False ys



-- The right fold, foldr works in a similar way to the left fold, only the accumulator eats up the values from the right
-- the left fold's binary function has the accumulator as the first parameter and the current value as the second one (so \acc x -> ...), the right fold's binary function has the current value as the first parameter and the accumulator as the second one (so \x acc -> ...)
-- One big difference is that right folds work on infinite lists, whereas left ones don't! To put it plainly, if you take an infinite list at some point and you fold it up from the right, you'll eventually reach the beginning of the list. However, if you take an infinite list at a point and you try to fold it up from the left, you'll never reach an end
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs



-- The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They cause runtime errors if called with empty lists.
sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+) 


maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> max acc x)


reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []


reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) [] 


product' :: (Num a) => [a] -> a
product' = foldl (*) 1


filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []


head' :: [a] -> a
head' = foldr1 (\x _ -> x)


last' :: [a] -> a
last' = foldl1 (\_ x -> x)
