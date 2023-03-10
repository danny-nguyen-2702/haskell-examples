{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}
{-# LANGUAGE InstanceSigs #-}

data Box a = EmptyBox | Has a
data Present t a= EmptyPresent t | PresentFor t a

class Container c where
  isEmpty :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace :: c a -> b -> c b
  unwrap :: c a -> a -> a

instance Container Box where
  isEmpty :: Box a -> Bool
  isEmpty EmptyBox = True
  isEmpty (Has a) = False

  contains :: (Eq a) => Box a -> a -> Bool
  contains EmptyBox x = False
  contains (Has y) x = x == y

  replace :: Box a -> b -> Box b
  replace EmptyBox x = Has x
  replace (Has y) x = Has x

  unwrap :: Box a -> a -> a
  unwrap (Has x) _ = x
  unwrap EmptyBox def = def

instance Container (Present t) where
  isEmpty :: Present t a -> Bool
  isEmpty (EmptyPresent t) = True
  isEmpty (PresentFor t a) = False

  contains :: (Eq a) => Present t a -> a -> Bool
  contains (EmptyPresent t) _ = False
  contains (PresentFor t a ) x = a == x

  replace :: Present t a -> b -> Present t b
  replace (EmptyPresent t) y = PresentFor t y
  replace (PresentFor t a) y = PresentFor t y

  unwrap :: Present t a -> a -> a
  unwrap (PresentFor t x) _ = x
  unwrap (EmptyPresent t) def = def

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

instance Container (MailedBox t d) where
  isEmpty :: MailedBox t d a -> Bool
  isEmpty (EmptyMailBox t d) = True
  isEmpty _ = False

  contains :: (Eq a) => MailedBox t d a -> a -> Bool
  contains (EmptyMailBox t d) x = False
  contains (MailBoxTo t d a) x = x == a

  replace :: MailedBox t d a -> b -> MailedBox t d b
  replace (EmptyMailBox t d) y = MailBoxTo t d y
  replace (MailBoxTo t d x) y = MailBoxTo t d y

  unwrap :: MailedBox t d a -> a -> a
  unwrap (MailBoxTo t d x) _ = x
  unwrap (EmptyMailBox t d) def = def
  
-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq)

instance Ord Experience where
  compare Programming Programming = EQ
  compare Managing Managing = EQ
  compare Leading Leading = EQ
  compare Programming _ = LT
  compare Leading _ = GT
  compare Managing Programming = GT
  compare Managing Leading = LT

type Address = String

data Salary = USD Double | EUR Double deriving (Show)

instance Eq Salary where
  (USD x) == (EUR y) = x == 0.94 * y
  (EUR x) == (USD y) = 0.94 * x == y
  (USD x) == (USD y) = x == y
  (EUR x) == (EUR y) = x == y 

instance Ord Salary where
  compare (USD x) (USD y) = x `compare` y
  compare (EUR x) (EUR y) = x `compare` y
  compare (USD x) (EUR y) = x `compare` (0.94 * y)
  compare (EUR x) (USD y) = (0.94 * x) `compare` y

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show, Eq)

relationship1 = Employee Intern Programming (USD 1000) "Vietnam"
relationship2 = Employee Intern Programming (USD 1000) "USA"

instance Ord Relationship where
  compare :: Relationship -> Relationship -> Ordering
  compare (Contractor p1 e1 s1 a1) (Contractor p2 e2 s2 a2) = s1 `compare` s2
  compare (Employee p1 e1 s1 a1) (Employee p2 e2 s2 a2) = s1 `compare` s2
  compare (Employee _ _ _ _) (Contractor _ _ _ _) = GT
  compare (Contractor _ _ _ _) (Employee _ _ _ _) = LT


data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Show, Eq)

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

instance Ord Pokemon where
  compare Pokemon {pPokeDexNum=p1} Pokemon {pPokeDexNum=p2} = compare p1 p2

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show)

-- Team memeber data
type TeamMember = (String, Exp)

instance Num Exp where
  (Exp x) + (Exp y) = Exp (x + y)
  (Exp x) - (Exp y) = Exp (x - y)
  (Exp x) * (Exp y) = Exp (x * y)
  
  abs (Exp x) = Exp (abs x)

  signum (Exp x) = Exp (signum x)

  fromInteger x = Exp (fromInteger x)


-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
