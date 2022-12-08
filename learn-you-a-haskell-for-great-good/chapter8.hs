---------------------- MAKING OUR OWN TYPES AND TYPECLASSES ----------------------
import qualified Data.Map as Map
import Prelude hiding (fmap)

-- A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes.

-- We have 2 kinds of type. The first one is type of data (Bool, Int, Char, Maybe, etc.) and the second one is type of function (a -> b)

-- To define our own data type, we can ise the data keyword.
data Bool' = False' | True'
-- data means that we're defining a new data type. The part before the = denotes the type, which is Bool. The parts after the = are value constructors. They specify the different values that this type can have. The | is read as or.

-- Value constructors are actually functions that ultimately return a value of a data type. So when we write a value constructor, we can optionally add some types after it that act as its parameters and those types define the values it will contain
-- The following Circle value contructor is a fuction that takes 3 Float and returns a Shape
-- We add deriving (Show) at the end of a data declaration, Haskell automagically makes that type part of the Show typeclass. So we can print out Circle 10 20 5 in the prompt without getting errors.
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)


-- Let's make a function that takes a shape and returns its surface
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r * 2
-- surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)
-- The first notable thing here is the type declaration. It says that the function takes a shape and returns a float. We couldn't write a type declaration of Circle -> Float because Circle is not a type, Shape is. Just like we can't write a function with a type declaration of True -> Int.
-- The next thing we notice here is that we can pattern match against constructors. Just like we pattern matches with True and False when the input type is Bool



-- Value constructors are functions, so we can map them and partially apply them and everything. If we want a list of concentric circles with different radii, we can do this.
-- concentricCircles = map (Circle 10 20) [4,5,6,7]  



-- Let's make an intermediate data type that defines a point in two-dimensional space. Then we can use that to make our shapes more understandable.
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Let's make a nudge function. It takes a shape, the amount to move it on the x axis and the amount to move it on the y axis and then returns a new shape that has the same dimensions
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r ) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- If we don't want to deal directly with points, we can make some auxilliary functions that create shapes of some size at the zero coordinates
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)



-- If we wanted to export the functions and types that we defined here in a module, we could start it off like this (it shoule be defined at the beginning of this file)
{-
module Shape (
    Point (..),
    Shape (..),
    surface,
    nudge,
    baseCircle,
    baseRect
) where
-}
-- By doing Shape(..), we exported all the value constructors for Shape, so that means that whoever imports our module can make shapes by using the Rectangle and Circle value constructors. It's the same as writing Shape (Rectangle, Circle)



-- We can define our own data type in different way using record syntax.
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
} deriving (Show)
-- By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor for you to lookup fields in your data type



-------------------- Type contructors and Type parameters --------------------

-- Type contructor is not value contructor. A value constructor can take some values parameters and then produce a new value. For instance, the Car constructor takes three values and produces a car value. In a similar manner, type constructors can take types as parameters to produce new types
data Maybe' a = Nothing' | Just' a

-- See the above example, the a here is the type parameter. And because there's a type parameter involved, we call Maybe a type constructor. So if we pass Char as the type parameter to Maybe, we get a type of Maybe Char. 
-- It's very important to distinguish between the type constructor and the value constructor. When declaring a data type, the part before the = is the type constructor and the constructors after it (possibly separated by |'s) are value constructors

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- These above functions can operate on types of Vector Int, Vector Integer, Vector Float, whatever, as long as the a from Vector a is from the Num typeclass


-------------------- Derived instances --------------------

-- A typeclass is a sort of an interface that defines some behavior. A type can be made an instance of a typeclass if it supports that behavior.

-- We also mentioned that they're often confused with classes in languages like Java, Python, C++ and the like, which then baffles a lot of people. In those languages, classes are a blueprint from which we then create objects that contain state and can do some actions. Typeclasses are more like interfaces. We don't make data from typeclasses. Instead, we first make our data type and then we think about what it can act like. If it can act like something that can be equated, we make it an instance of the Eq typeclass. If it can act like something that can be ordered, we make it an instance of the Ord typeclass.

-- Haskell can automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read. Haskell can derive the behavior of our types in these contexts if we use the deriving keyword when making our data type.
data Member = Member {
    name :: String,
    code :: Int
} deriving (Eq, Show, Read)

{-
ghci> let mikeD = Member {name = "Michael", code = 43}  
ghci> let adRock = Member {name = "Adam", code = 41}  
ghci> let mca = Member {name = "Adam", code = 44}  
ghci> let beastieBoys = [mca, adRock, mikeD]  
ghci> mikeD `elem` beastieBoys  
True 

ghci> read "Member {name = \"Adam\", code = 44}" :: Member
Member {name = "Adam", code = 44} 
-}


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

{-
Because it's part of the Show and Read typeclasses, we can convert values of this type to and from strings.

ghci> Wednesday  
Wednesday  
ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday

Because it's part of the Eq and Ord typeclasses, we can compare or equate days.

ghci> Saturday == Sunday  
False  
ghci> Saturday == Saturday  
True  
ghci> Saturday > Friday  
True  
ghci> Monday `compare` Wednesday  
LT


It's also part of Bounded, so we can get the lowest and highest day.

ghci> minBound :: Day  
Monday  
ghci> maxBound :: Day  
Sunday

It's also an instance of Enum. We can get predecessors and successors of days and we can make list ranges from them!

ghci> succ Monday  
Tuesday  
ghci> pred Saturday  
Friday  
ghci> [Thursday .. Sunday]  
[Thursday,Friday,Saturday,Sunday]  
ghci> [minBound .. maxBound] :: [Day]  
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] 
-}


-------------------- Type synonyms --------------------

-- Use type synonyms to give some types different names so that they make more sense to someone reading our code and documentation.

-- We've introduced the type keyword. The keyword might be misleading to some, because we're not actually making anything new (we did that with the data keyword), but we're just making a synonym for an already existing type.

type Company = [Member]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- Type synonyms can also be parameterized. If we want a type that represents an association list type but still want it to be general so it can use any type as the keys and values, we can do this:
type AssocList k v = [(k,v)]


-- Another cool data type that takes two types as its parameters is the Either a b type. This is roughly how it's defined:
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  


-- An example: a high-school has lockers so that students have some place to put their Guns'n'Roses posters. Each locker has a code combination. When a student wants a new locker, they tell the locker supervisor which locker number they want and he gives them the code. However, if someone is already using that locker, he can't tell them the code for the locker and they have to pick a different one. We'll use a map from Data.Map to represent the lockers. It'll map from locker numbers to a pair of whether the locker is in use or not and the locker code.



data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerNumber = Int

type LockerMap = Map.Map LockerNumber (LockerState, Code)

-- And now, we're going to make a function that searches for the code in a locker map
-- We're going to use an Either String Code type to represent our result, because our lookup can fail in two ways — the locker can be taken, in which case we can't tell the code or the locker number might not exist at all. If the lookup fails

lockerLookup :: LockerNumber -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap =
    case Map.lookup lockerNumber lockerMap of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) ->
            if state Prelude.== Taken
            then Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
            else Right code

lockers :: LockerMap
lockers = Map.fromList [
    (100, (Taken, "ABC123")),
    (101, (Free, "SNO2AA")),
    (102, (Free, "KVN93N")),
    (103, (Free, "CKLE13")),
    (104, (Taken, "FLR2N1")),
    (105, (Taken, "O223PC"))
    ]

-- ghci> lockerLookup 101 lockers
-- Right "SNO2AA"
-- ghci> lockerLookup 100 lockers
-- Left "Locker 100 is already taken!"
-- ghci> lockerLookup 110 lockers
-- Left "Locker number 110 doesn't exist!"


-------------------- Recursive data structures --------------------

-- We can make types whose constructors have fields that are of the same type! Using that, we can create recursive data types, where one value of some type contains values of that type, which in turn contain more values of the same type and so on.

-- We could say that a list can be an empty list or it can be an element joined together with a : with another list (that can be either the empty list or not). 
-- You might also be confused about the Cons constructor here. cons is another word for (:). You see, in lists, : is actually a constructor that takes a value and another list and returns a list.
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- ghci> Empty  
-- Empty  
-- ghci> 5 `Cons` Empty  
-- Cons 5 Empty  
-- ghci> 4 `Cons` (5 `Cons` Empty)  
-- Cons 4 (Cons 5 Empty)  
-- ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
-- Cons 3 (Cons 4 (Cons 5 Empty))  



-- We can define functions to be automatically infix by making them comprised of only special characters. We can also do the same with constructors, since they're just functions that return a data type. So in the following custom List type, we use :-: as one of our contructors.
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- First off, we notice a new syntactic construct, the fixity declarations. When we define functions as operators, we can use that to give them a fixity (but we don't have to). A fixity states how tightly the operator binds and whether it's left-associative or right-associative. 
-- Now, we can write out lists in our list type like so: 3 :-: 4 :-: 5 :-: Empty


-- Let's make a function that adds two of our lists together. Notice how we pattern matched on (x :-: xs). That works because pattern matching is actually about matching constructors. Same goes for []. Because pattern matching works (only) on constructors, we can match for stuff like that, normal prefix constructors or stuff like 8 or 'a', which are basically constructors for the numeric and character types, respectively.
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- ghci> x = 3 :-: 4 :-: Empty
-- ghci> y = 5 :-: 6 :-: 7 :-: Empty
-- ghci> x .++ y
-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))



-- Now, we're going to implement a binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)
singletonTree :: (Ord a) => a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a l r)
    | x Prelude.== a = Node a l r
    | x < a = Node a (treeInsert x l) r
    | x > a = Node a l (treeInsert x r)

treeFind :: (Ord a) => a -> Tree a -> Bool
treeFind x EmptyTree = False
treeFind x (Node a l r)
    | x Prelude.== a = True
    | x < a = treeFind x l
    | x > a = treeFind x r

ourTree = foldr treeInsert EmptyTree [7, 3, 2, 9, 5, 0, 2, 7, 5, 6, 3, 4]




--------------------------- Define our own typeclasses ---------------------------
-- A quick recap on typeclasses: typeclasses are like interfaces. A typeclass defines some behavior (like comparing for equality, comparing for ordering, enumeration) and then types that can behave in that way are made instances of that typeclass

-- The behavior of typeclasses is achieved by defining functions or just type declarations that we then implement. So when we say that a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with that type.

-- Typeclasses have pretty much nothing to do with classes in languages like Java or Python. In those languages, classes are a blueprint from which we then create objects that contain state and can do some actions. Typeclasses are more like interfaces. We don't make data from typeclasses.

-- This is how the Eq class is defined in the standard prelude:
class Eq' a where
    (==) :: a -> a -> Bool  -- this is type declaration
    (/=) :: a -> a -> Bool  -- this is type declaration
    x == y = not (x Main./= y)   -- this is function body, but it's not mandatory
    x /= y = not (x Main.== y)   -- this is function body, but it's not mandatory

data TrafficLight = Red | Yellow | Green

-- after we defined our typeclass Eq', we can make the data type TrafficLight an instance of our typeclass Eq' by using the instance keyword

instance Eq' TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

-- So class keyword is for defining new typeclasses and data keyword is for defining new data types and instance keyword is for making our types instances of typeclasses.

-- You can also make typeclasses that are subclasses of other typeclasses.
-- class (Eq a) => Num a where 
--     ...
-- We're essentially saying that we have to make a type an instance of Eq before we can make it an instance of Num. Before some type can be considered a number, it makes sense that we can determine whether values of that type can be equated or not. That's all there is to subclassing really, it's just a class constraint on a class declaration




-- But how are the Maybe or list types made as instances of typeclasses? What makes Maybe different from, say, TrafficLight is that Maybe in itself isn't a concrete type, it's a type constructor that takes one type parameter (like Char or something) to produce a concrete type (like Maybe Char)
-- From the type declarations (inside class declarations), we see that the type variable `a` is used as a concrete type because all the types in functions have to be concrete (remember, you can't have a function of the type a -> Maybe but you can have a function of a -> Maybe a or Maybe Int -> Maybe String). That's why we can't do something like:
-- instance Eq Maybe' where
--     ...

-- It would also be tedious to write instance Eq (Maybe' Int) where, instance Eq (Maybe' Char) where, etc. for every type ever. So we could write it out like so:
instance (Eq m) => Eq (Maybe' m) where
    Just' x == Just' y = x Prelude.== y
    Nothing' == Nothing' = True
    _ == _ = False
-- With this instance declaration, we say this: we want all types of the form Maybe' m to be part of the Eq typeclass, but only those types where the m (so what's contained inside the Maybe') is also a part of Eq
-- Most of the times, class constraints in class declarations are used for making a typeclass a subclass of another typeclass and class constraints in instance declarations are used to express requirements about the contents of some type. For instance, here we required the contents of the Maybe' to also be part of the Eq typeclass.




------------------ A yes-no typeclass example ------------------
-- In JavaScript and some other weakly typed languages, you can put almost anything inside an if expression. For example, you can do all of the following: if (0) alert("YEAH!") else alert("NO!"), if ("") alert ("YEAH!") else alert("NO!"), if (false) alert("YEAH") else alert("NO!), etc. and all of these will throw an alert of NO!
-- Even though strictly using Bool for boolean semantics works better in Haskell, let's try and implement that JavaScript-ish behavior anyway

-- The YesNo typeclass defines one function. That function takes one value of a type that can be considered to hold some concept of true-ness and tells us for sure if it's true or not. Notice that from the way we use the a in the function, a has to be a concrete type.
class YesNo a where
    yesno :: a -> Bool

-- For numbers, we'll assume that (like in JavaScript) any number that isn't 0 is true-ish and 0 is false-ish
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- Empty lists (and by extensions, strings) are a no-ish value, while non-empty lists are a yes-ish value.
-- Notice how we just put in a type parameter a in there to make the list a concrete type, even though we don't make any assumptions about the type that's contained in the list
instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id -- id is just a standard library function that takes a parameter and returns the same thing

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- let's go play!
-- ghci> yesno $ length []  
-- False  
-- ghci> yesno "haha"  
-- True  
-- ghci> yesno ""  
-- False  
-- ghci> yesno $ Just 0  
-- True  
-- ghci> yesno True  
-- True  
-- ghci> yesno EmptyTree  
-- False  
-- ghci> yesno []  
-- False  
-- ghci> yesno [0,0,0]  
-- True  
-- ghci> :t yesno  
-- yesno :: (YesNo a) => a -> Bool  



------------------ The Functor typeclass ------------------
-- Now we're going to take a look at the Functor typeclass, which is basically for things that can be mapped over. You're probably thinking about lists now, since mapping over lists is such a dominant idiom in Haskell. And you're right, the list type is part of the Functor typeclass

class Functor' f where
    fmap :: (a -> b) -> f a -> f b

-- Alright. We see that it defines one function, fmap. The type of fmap is interesting. In the definitions of typeclasses so far, the type variable that played the role of the type in the typeclass was a concrete type, like the a in (==) :: (Eq a) => a -> a -> Bool. But now, the f is not a concrete type, but a type constructor that takes one type parameter. A quick refresher example: Maybe Int is a concrete type, but Maybe is a type constructor that takes one type as the parameter. Anyway, we see that fmap takes a function from one type to another and a functor applied with one type and returns a functor applied with another type.

-- For example, the type signature of map is map :: (a -> b) -> [a] -> [b]. It takes a function from one type to another and a list of one type and returns a list of another type. In fact, map is just a fmap that works only on lists. Here's how the list is an instance of the Functor typeclass.

instance Functor' [] where
    fmap = map

-- That's it! Notice how we didn't write instance Functor [a] where, because from the Functor' class declaration, we see that the f has to be a type constructor that thats one type. [a] is already a concrete type, while [] is a type constructor that takes one type and can produce types such as [Int], [String] or even [[String]]

-- What happens when we map or fmap over an empty list? Well, of course, we get an empty list. It just turns an empty list of type [a] into an empty list of type [b]


-- Types that can act like a box can be functors. You can think of a list as a box that has an infinite amount of little compartments and they can all be empty, one can be full and the others empty or a number of them can be full. So, what else has the properties of being like a box? For one, the `Maybe a` type. In a way, it's like a box that can either hold nothing, or it can hold one item.

-- When we fmap a function with a `Maybe a` type, it turns the `Maybe a` value into Maybe (function a) value. The type constructor still stay the same (which is Maybe in this case), but the value hold inside the Maybe constructor has been changed. That's what we see in the fmap declaration (fmap :: (a -> b) -> f a -> f b), the f constructor stays the same but the value inside has been changed from a to b

instance Functor' Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap function (Just a) = Just (function a)
    fmap function Nothing = Nothing

-- Again, notice how we wrote `instance Functor Maybe where` instead of `instance Functor (Maybe m) where`. Functor wants a type constructor that takes one type and not a concrete type. Anyway, the fmap implementation is pretty simple. If it's an empty value of Nothing, then just return a Nothing. If we map over an empty box, we get an empty box. It makes sense. Just like if we map over an empty list, we get back an empty list. If it's not an empty value, but rather a single value packed up in a Just, then we apply the function on the contents of the Just.



-- Another thing that can be mapped over and made an instance of Functor is our Tree a type. It can be thought of as a box in a way (holds several or no values) and the Tree type constructor takes exactly one type parameter.
-- Mapping over an empty tree will produce an empty tree. Mapping over a non-empty tree will be a tree consisting of our function applied to the root value and its left and right sub-trees will be the previous sub-trees, only our function will be mapped over them.
instance Functor' Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node a l r) = Node (f a) (Main.fmap f l) (Main.fmap f r)

-- We can test this fmap function for our Tree in ghci using this command ghci> fmap (*2) ourTree



-- Now how about Either a b? Can this be made a functor? The Functor typeclass wants a type constructor that takes only one type parameter but Either takes two. Hmmm! I know, we'll partially apply Either by feeding it only one parameter so that it has one free parameter. Here's how Either a is a functor in the standard libraries:
instance Functor' (Either a) where
    -- fmap :: (b -> c) -> Either a b -> Either a c
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
--  You can see how we made Either a an instance instead of just Either. That's because Either a is a type constructor that takes one parameter, whereas Either takes two. If fmap was specifically for Either a, the type signature would then be (b -> c) -> Either a b -> Either a c because that's the same as (b -> c) -> (Either a) b -> (Either a) c

-- Maps from Data.Map can also be made a functor because they hold values (or not!). In the case of Map k v, fmap will map a function v -> v' over a map of type Map k v and return a map of type Map k v'
instance (Ord k) => Functor' (Map.Map k) where
    -- fmap :: (v -> v') -> Map.Map k v -> Map.Map k v'
    -- fmap f m = Map.fromList $ [(k, f v) | (k, v) <- Map.toList m]
    fmap f m = Map.foldrWithKey (\k v acc -> Map.insert k (f v) acc) Map.empty m

