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

-- partition takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't. For example: ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" returns ("BOBMORGAN","sidneyeddy")


-- isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively. 

-- find takes a list and a predicate and returns the first element that satisfies the predicate. But it returns that element wrapped in a Maybe value. For example: ghci> find (>4) [1,2,3,4,5,6] returns Just 5


-- elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index of the element we're looking for. If that element isn't in our list, it returns a Nothing

-- elemIndices is like elemIndex, only it returns a list of indices, in case the element we're looking for crops up in our list several times


-- findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate. findIndices returns the indices of all elements that satisfy the predicate in the form of a list.


-- lines is a useful function when dealing with files or input from somewhere. It takes a string and returns every line of that string in a separate list. For example: ghci> lines "first line\nsecond line\nthird line" returns ["first line","second line","third line"]  

-- unlines is the inverse function of lines. It takes a list of strings and joins them together using a '\n'. For example: ghci> unlines ["first line", "second line", "third line"] returns "first line\nsecond line\nthird line\n"

-- words and unwords are for splitting a line of text into words or joining a list of words into a text.

-- delete takes an element and a list and deletes the first occurence of that element in the list. For example: ghci> delete 'h' "hey there ghang!" returns "ey there ghang!"


-- \\ is the list difference function. It acts like a set difference, basically. For every element in the right-hand list, it removes a matching element in the left one. For example: ghci> [1..10] \\ [2,5,9] returns [1,3,4,6,7,8,10] 


-- union also acts like a function on sets. It returns the union of two lists. It pretty much goes over every element in the second list and appends it to the first one if it isn't already in yet. Watch out though, duplicates are removed from the second list! For example: ghci> [1..7] `union` [5..10] returns [1,2,3,4,5,6,7,8,9,10] 


-- intersect works like set intersection. It returns only the elements that are found in both lists. For example: ghci> [1..7] `intersect` [5..10] returns [5,6,7]  


-- insert takes an element and a list of elements that can be sorted and inserts it into the last position where it's still less than or equal to the next element. For example: ghci> insert 4 [3,5,1,2,8,2] returns [3,4,5,1,2,8,2]  


-- The nub, delete, union, intersect and group functions all have their more general counterparts called nubBy, deleteBy, unionBy, intersectBy and groupBy. The difference between them is that the first set of functions use == to test for equality, whereas the By ones also take an equality function and then compare them by using that equality function. group is the same as groupBy (==)


-- Similarly, the sort, insert, maximum and minimum also have their more general equivalents. Functions like groupBy take a function that determines when two elements are equal. sortBy, insertBy, maximumBy and minimumBy take a function that determine if one element is greater, smaller or equal to the other. 




import qualified Data.Map as Map 

-- The fromList function takes an association list (in the form of a list) and returns a map with the same associations. For example: ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] returns fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 


-- empty represents an empty map. It takes no arguments, it just returns an empty map. For example: ghci> Map.empty returns fromList [] 


-- insert takes a key, a value and a map and returns a new map that's just like the old one, only with the key and value inserted. For example: ghci> Map.insert 3 100 Map.empty returns fromList [(3,100)] 

-- null checks if a map is empty. For example: ghci> Map.null Map.empty returns True 


-- size reports the size of a map. For example: Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] returns 5


-- singleton takes a key and a value and creates a map that has exactly one mapping. For example: ghci> Map.singleton 3 9 returns fromList [(3,9)] 


-- lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't


-- member is a predicate takes a key and a map and reports whether the key is in the map or not. For example: ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] returns True  


-- map and filter work much like their list equivalents. For example: ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)] returns fromList [(1,100),(2,400),(3,900)]


-- toList is the inverse of fromList. For example: ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3 returns [(4,3),(9,2)]  


-- keys and elems return lists of keys and values respectively. keys is the equivalent of map fst . Map.toList and elems is the equivalent of map snd . Map.toList


-- fromListWith is a cool little function. It acts like fromList, only it doesn't discard duplicate keys but it uses a function supplied to it to decide what to do with them
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs  


-- insertWith is to insert what fromListWith is to fromList. It inserts a key-value pair into a map, but if that map already contains the key, it uses the function passed to it to determine what to do. For example: ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)] returns fromList [(3,104),(5,103),(6,339)]  
