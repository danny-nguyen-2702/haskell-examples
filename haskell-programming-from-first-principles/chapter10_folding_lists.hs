import Data.Time
import Data.List
data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase = [ 
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!", 
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]


-- Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f x acc =
            case x of
                (DbDate utcTime) -> utcTime : acc
                _ -> acc


-- Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f x acc =
            case x of
                (DbNumber val) -> val : acc
                _ -> acc
            

-- Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate


-- Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber


-- Write a function that gets the average of the DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb xs = realToFrac (sumDb xs) / genericLength (filterDbNumber xs)


-- Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will

svsCombinations :: String -> String -> [(Char, Char, Char)]
svsCombinations stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops]


-- b) Modify that function so that it only returns the combinations that begin with a p.
svsCombinations' :: String -> String -> [(Char, Char, Char)]
svsCombinations' stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']


-- c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences

nouns :: [String]
nouns = ["noun1", "noun2", "noun3"]
verbs :: [String]
verbs = ["verb1", "verb2", "verb3"]

nvnSentences :: [String] -> [String] -> [String]
nvnSentences nouns verbs = [x ++ " " ++  y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns]


-- get average numbers of characters per word
seekritFunc :: String -> Double
seekritFunc x = (realToFrac . sum . map length . words) x / genericLength (words x)


---------- Rewriting functions using folds ----------

myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False


myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)


myElem' :: Eq a => a -> [a] -> Bool
myElem' a = foldr (\x acc -> x == a || acc) False


myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []


-- squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f


-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compare xs = head $ foldr f [] xs
    where f x acc 
            | null acc = [x]
            | compare x (head acc) == GT = [x]
            | otherwise = acc