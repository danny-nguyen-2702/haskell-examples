{-# LANGUAGE FlexibleInstances #-}

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