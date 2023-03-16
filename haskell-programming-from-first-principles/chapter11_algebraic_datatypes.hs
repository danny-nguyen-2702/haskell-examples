{-# LANGUAGE FlexibleInstances #-}
import Data.Char (toUpper)

data Price = Price Integer deriving (Show, Eq)

data Manufacturer = Mini | Mazda | Tata deriving (Show, Eq)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Show, Eq)

type Size = String
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Show, Eq)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir "Big Size"


-- 1. What is the type of myCar?
-- Vehicle

-- 2. Given the following, define the functions:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Now we’re going to write a function to tell us the manufacturer of a piece of data:

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?
-- bottom

-- 5. All right. Let’s say you’ve decided to add the size of the plane as an argument to the Plane constructor. Add that to your datatypes in the appropriate places and change your data and functions appropriately.


class TooMany a where
    tooMany :: a -> Bool
    
instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
    tooMany (Goats n) = tooMany n

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass for the type (Int, String). This will require adding a language pragma named FlexibleInstances if you do not use a newtype — GHC will tell you what to do.

instance TooMany (Int, String) where
    tooMany (x, y) = tooMany x


-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption this is a count of goats from two fields.

instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany $ Goats (x+y)

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such as summing the two numbers together.

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x+y)






data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer = Programmer { 
    os :: OperatingSystem,
    lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Write a function that generates all possible values of Programmer. Use the provided lists of inhabitants of OperatingSystem and ProgLang.

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = os, lang = lang} | os <- allOperatingSystems, lang <- allLanguages]



---------- Binary Tree ----------

data BinaryTree a = 
    Leaf 
    | Node (BinaryTree a) a (BinaryTree a) 
    deriving (Show, Eq, Ord)

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf = Node Leaf x Leaf
insertTree x (Node left y right)
    | x < y = Node (insertTree x left) y right
    | x > y = Node left y (insertTree x right)
    | otherwise = Node left y right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

-- Write functions to convert BinaryTree values to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = preorder left ++ [x] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = preorder right ++ [x] ++ preorder left

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


-- Write foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left x right) = foldTree f (f x (foldTree f acc right)) left

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' f acc tree = foldr f acc (inorder tree)


-- Use as-patterns in implementing the following functions

-- 1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous. Remember that the sub-sequence has to be in the original order!
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf allX@(x:xs) (y:ys) 
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf allX ys

-- 2. Split a sentence into words, then tuple each word with the capitalized form of each
capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map (\word@(x:xs) -> (word, toUpper x : xs)) $ words str



-- Language exercises
-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord function