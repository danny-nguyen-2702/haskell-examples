-------------------------------- MODULES --------------------------------

-- A Haskell module is a collection of related functions, types and typeclasses. A Haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something.
-- The Haskell standard library is split into modules, each of them contains functions and types that are somehow related and serve some common purpose. 
-- The syntax for importing modules in a Haskell script is import <module name>
-- If you just need a couple of functions from a module, you can selectively import just those functions. For example: import Data.List (nub, sort)
-- You can also choose to import all of the functions of a module except a few select ones. That's often useful when several modules export functions with the same name and you want to get rid of the offending ones. For example: import Data.List hiding (nub) 
-- Another way of dealing with name clashes is to do qualified imports. For example: import qualified Data.Map as M 


import Data.List


-- nub is a function defined in Data.List that takes a list and removes duplicate elements. In particular, it keeps only the first occurrence of each element. (The name nub means `essence')
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) 
    | x `elem` xs = nub' xs
    | otherwise = x : nub' xs

nub'' :: (Eq a) => [a] -> [a]
nub'' = foldr (\x acc -> if x `elem` acc then acc else x:acc) []


-- Data.List.intersperse takes an element and a list and then puts that element in between each pair of elements in the list. For example: ghci> intersperse '.' "MONKEY" returns "M.O.N.K.E.Y"  
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' i (x:xs) = x : i : intersperse' i xs


-- Data.List.intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result. For example: ghci> intercalate " " ["hey","there","guys"] returns "hey there guys"
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' _ [x] = x
intercalate' i (x:xs) = x ++ i ++ intercalate' i xs


-- foldl' and foldl1' are stricter versions of their respective lazy incarnations. When using lazy folds on really big lists, you might often get a stack overflow error. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens. What actually happens is that the accumulator kind of makes a promise that it will compute its value when asked to actually produce the result (also called a thunk). That happens for every intermediate accumulator and all those thunks overflow your stack. The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks. So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.


-- concat flattens a list of lists into just a list of elements. For example: ghci> concat ["foo","bar","car"] returns "foobarcar"


-- and takes a list of boolean values and returns True only if all the values in the list are True. For example: and $ map (>4) [5,6,7,8] return True
and' :: [Bool] -> Bool
and' = foldr (&&) True 


-- or is like and, only it returns True if any of the boolean values in a list is True
or'  :: [Bool] -> Bool
or' = foldr (||) False

-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> acc || f x) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\x acc -> acc && f x) True

-- iterate takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list. For example: ghci> take 10 $ iterate (*2) 1 returns [1,2,4,8,16,32,64,128,256,512]

-- splitAt takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple. For example: ghci> splitAt 3 "heyman" returns ("hey","man")

-- takeWhile is a really useful little function. It takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate, it's cut off. For example: ghci> takeWhile (/=' ') "This is a sentence" returns "This" 

-- dropWhile is similar, only it drops all the elements while the predicate is true. Once predicate equates to False, it returns the rest of the list. For example: ghci> dropWhile (/=' ') "This is a sentence" returns " is a sentence"


-- span is kind of like takeWhile, only it returns a pair of lists. The first list contains everything the resulting list from takeWhile would contain if it were called with the same predicate and the same list. The second list contains the part of the list that would have been dropped. For example: ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest returns "First word: This, the rest: is a sentence

-- Whereas span spans the list while the predicate is true, break breaks it when the predicate is first true. Doing break p is the equivalent of doing span (not . p). For example: ghci> break (==4) [1,2,3,4,5,6,7] returns ([1,2,3],[4,5,6,7])

-- sort simply sorts a list. The type of the elements in the list has to be part of the Ord typeclass


-- group takes a list and groups adjacent elements into sublists if they are equal. For example: ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] returns [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]


-- inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left. For example: ghci> let w = "w00t" in zip (inits w) (tails w) returns [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")] 


-- isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for is somewhere inside the target list. For example: ghci> "cat" `isInfixOf` "im a cat burglar" returns True 
