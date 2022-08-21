import Data.Char

-- Write your own enumFromTo definitions for the types provided. Do not use range syntax to do so. It should return the same results as if you did [start..stop]

enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' start stop
    | start > stop = []
    | start == stop = [stop]
    | otherwise = start : enumFromTo (succ start) stop


-- Using takeWhile and dropWhile, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample:
-- Prelude> myWords "sheryl wants fun"
-- ["sheryl", "wants", "fun"]

myWords :: String -> [String]
myWords "" = []
myWords (' ':xs) = myWords xs
myWords str = takeWhile (/=' ') str : myWords (dropWhile (/= ' ') str)



-- write a function that takes a string and returns a list of strings, using newline separators to break up the string as in the following
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n':xs) = myLines xs
myLines str = takeWhile (/='\n') str : myLines (dropWhile (/= '\n') str)


-- Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite myWords and myLines using it

stringSeparator :: Char -> String -> [String]
stringSeparator _ [] = []
stringSeparator separator str = if head str == separator 
    then stringSeparator separator (tail str)
    else takeWhile (/=separator) str : stringSeparator separator (dropWhile (/=separator) str)

myLines' :: String -> [String]
myLines' = stringSeparator '\n'

myWords' :: String -> [String]
myWords' = stringSeparator ' '



-- Given the following:
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- First write an expression that will make tuples of the outputs of mySqr and myCube
myTuples = [(x, y) | x <- mySqr, y <- myCube]

-- Now alter that expression so that it only uses the x and y values that are less than 50
myTuples' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Apply another function to that list comprehension to determine how many tuples inhabit your output list
howManyTuples = length myTuples'




-- write a filter function that would give us all the multiples of 3 out of a list from 1-30
multiplesOf3 :: [Integer] -> [Integer]
multiplesOf3 = filter (\x -> x `mod` 3 == 0)

-- Recalling what we learned about function composition, how could we compose the above function with the length function to tell us *how many* multiples of 3 there are between 1 and 30?
numberOfMultiplesOf3 :: [Integer] -> Int
numberOfMultiplesOf3 = length . multiplesOf3


-- Next we’re going to work on removing all articles (’the’, ’a’, and ’an’) from sentences. You want to get to something that works like this
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]

myFilter :: String -> [String]
myFilter = filter (\x -> x `notElem` ["the", "a", "an"]) . myWords


-- Write your own version of zip and ensure it behaves the same as the original
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- Do what you did for zip, but now for zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Rewrite your zip in terms of the zipWith you wrote.
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)



-- write a function that filters all the uppercase letters out of a String
removeUppercase :: String -> String
removeUppercase = filter isUpper



-- Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument “julie,” it will return “Julie.”
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter xs
    | null xs = []
    | otherwise = (toUpper . head) xs : tail xs


-- Now make a new version of that function that is recursive such that if you give it the input “woot” it will holler back at you “WOOT.” The type signature won’t change, but you will want to add a base case.
capitalizeAllLetters :: String -> String
capitalizeAllLetters [] = []
capitalizeAllLetters (x:xs) = toUpper x : capitalizeAllLetters xs

capitalizeAllLetters' :: String -> String
capitalizeAllLetters' = map toUpper


-- To do the final exercise in this section, we’ll need another standard function for lists called head. Query the type of head and experiment with it to see what it does. Now write a function that will capitalize the first letter of a String and return only that letter as the result
capitalizeFirstLetter' :: String -> Maybe Char 
capitalizeFirstLetter' [] = Nothing
capitalizeFirstLetter' (x:xs) = Just $ toUpper x



-- Writing your own standard functions

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs 


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = (x == y) || myElem x ys


myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (xs: xss) = xs ++ squish xss


-- squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xss = squish $ map f xss

-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for. If you import maximumBy from Data.List, you’ll see the type is:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = if f x (myMaximumBy f xs) == GT then x else myMaximumBy f xs


-- myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = if f x (myMinimumBy f xs) == LT then x else myMinimumBy f xs


-- Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum. If you have GHC 7.10 or newer, you’ll see a type constructor that wants a Foldable instance instead of a list as has been the case for many functions so far.

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare