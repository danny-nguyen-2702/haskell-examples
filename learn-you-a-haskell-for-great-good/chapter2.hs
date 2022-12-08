--  some simple operators
add = 2 + 4

multiply = 4 * 3

subtract  = 2 - 3

division = 5 / 2

booleanAnd = True && False

booleanOr = True || False

compareEqual = 5 == 5

compareDiff = 5 /= 2

successor = succ 8

min' = min 2 3

max' = max 3.3 6

callAsPrefixFunction = div 92 10
callAsInfixFunction = 92 `div` 10


--  define some simple functions
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumer x = 
    if x < 100
    then x * 2
    else x -- note that the else part of if statement is mandatory in Haskell


-- play with list
{-

Things to remember about lists:
+ List only stores elements of the same type. We can't have a list that has a few integers and then an few characters.
+ List can contain lists. The lists within a list can be of different lengths but they can't be of different types, you can't have a list that has some lists of characters and some lists of numbers. 
+ Strings are just lists of characters. "hello" is just syntatic sugar for ['h','e','l','l','o']

-}

appendToList = [1, 2, 3] ++ [4, 5, 6]

prependToList = 1:[2, 3, 4]

indexingList = [9.4,33.2,96.2,11.2,23.25] !! 1 -- the indices start at 0

firstElement = head [1, 2, 3] -- get error when use with empty list

lastElement = last [1, 2, 3] -- get error when use with empty list

tailElements = tail [1, 2, 3] -- take a list and remove its first element

initialElements = init [1, 2, 3] -- take a list and remove its last element

lengthOfList = length [1, 2, 3]

isEmpty = null [] -- return True if list is empty

reverseList = reverse [1, 2, 3]

extractElementsFromBeginning = take 3 [1, 2, 3, 4, 5, 6, 7]

removeElementsFromBeginning = drop 3 [1, 2, 3, 4, 5, 6, 7]

maxInList = maximum [8,4,2,1,5,6]

minInList = minimum [8,4,2,1,5,6]

sumOfList = sum [8,4,2,1,5,6]

productOfList = product [8,4,2,1,5,6]

isExists = 4 `elem` [1, 2, 3, 4, 5]

rangeOfNumbers = [1..20]

rangeOfCharacters = ['a'..'z']

evenNumbers = [2,4..20]

thirdNumbers = [3,6..30]

infiniteList = [1..] -- Do not print the value of this list because it's endless. Use take function to get a faction of the lists

infiniteList' = cycle [1, 2, 3, 4]

infiniteList'' = repeat 5


-- list comprehension
doubleNumbers = [x*2 | x <- [1..10]]

doubleNumbersWithCondition = [x*2 | x <- [1..10], x*2 >= 12]

-- Let's say we want a comprehension that replaces each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!". If a number isn't odd, we throw it out of our list. For convenience, we'll put that comprehension inside a function so we can easily reuse it
boomBangs xs = [if x > 10 then "BANG!" else "BOOM!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase str = [c | c <- str, c `elem` ['A' .. 'Z']]

multiplePredicates = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

multipleInputLists = [ [x,y] | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- Let's remove all odd numbers without flattening the list.
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
removeOddNumbersOfNestedList = [ [x | x <- xs, even x] | xs <- xxs ]

-- Tuple

{-
Things to remember about tuple:
+ Type of tuple depends on how many components it has and the types of the components. Tuple of 2 integers are not the same type with tuple of 3 integers.
+ Unlike a list, a tuple can contain a combination of several types.
+ You can't compare two tuples of different sizes, whereas you can compare two lists of different sizes.
-}

firstComponent = fst ("Danny", 28) -- only works on pairs. Don't work on triples, 4-tuples, 5-tuples, etc.
secondComponent = snd ("Wow", False) -- only works on pairs. Don't work on triples, 4-tuples, 5-tuples, etc.

combineTwoLists = zip [1, 2, 3, 4, 5] [5, 5, 5, 5, 5] -- [(1,5),(2,5),(3,5),(4,5),(5,5)]

-- which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?
rightTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a+b+c == 24]
