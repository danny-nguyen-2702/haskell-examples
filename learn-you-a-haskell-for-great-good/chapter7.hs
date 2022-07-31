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