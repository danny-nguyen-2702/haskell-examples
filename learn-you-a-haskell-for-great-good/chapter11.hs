--------------------------------- Functors, Applicative Functors and Monoids ---------------------------------
import System.IO

-------- Functors redux --------

--  Functors are things that can be mapped over, like lists, Maybes, trees, and such. In Haskell, they're described by the typeclass Functor, which has only one typeclass method, namely fmap, which has a type of fmap :: (a -> b) -> f a -> f b. It says: give me a function that takes an a and returns a b and a box with an a (or several of them) inside it and I'll give you a box with a b (or several of them) inside it. It kind of applies the function to the element inside the box.

-- In this section, we'll take a look at two more instances of functor, namely IO and (->) r.

class Functor' f where
    fmap :: (a -> b) -> f a -> f b

instance Functor' (Either a) where
    -- fmap :: (b -> c) -> Either a b -> Either a c
    fmap f (Left a) = Left a
    fmap f (Right b) = Right (f b)

instance Functor' IO where
    -- fmap :: (a -> b) -> IO a -> IO b
    fmap f action = do 
        value <- action
        let newValue = f value
        return newValue

main2 = do
    line <- Main.fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"


-- map one function over a function has to produce a function
instance Functor' ((->) r) where
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = (\x -> f (g x)) -- fmap = (.)
    


-- When we first learned about curried functions, we said that all Haskell functions actually take one parameter. A function a -> b -> c actually takes just one parameter of type a and then returns a function b -> c, which takes one parameter and returns a c. 
-- In the same vein, if we write fmap :: (a -> b) -> (f a -> f b), we can think of fmap not as a function that takes one function and a functor and returns a functor, but as a function that takes a function and returns a new function that's just like the old one, only it takes a functor as a parameter and returns a functor as the result. It takes an a -> b function and returns a function f a -> f b. This is called lifting a function.

-- ghci> :t fmap (*2)  
-- fmap (*2) :: (Num a, Functor f) => f a -> f a  
-- ghci> :t fmap (replicate 3)  
-- fmap (replicate 3) :: (Functor f) => f a -> f [a]  

-- The expression fmap (*2) is a function that takes a functor f over numbers and returns a functor over numbers. That functor can be a list, a Maybe , an Either String, whatever

-- ghci> fmap (replicate 3) [1,2,3,4]  
-- [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
-- ghci> fmap (replicate 3) (Just 4)  
-- Just [4,4,4]  



-- Next up, we're going to look at the functor laws. In order for something to be a functor, it should satisfy some laws. All functors are expected to exhibit certain kinds of functor-like properties and behaviors. They should reliably behave as things that can be mapped over. Calling fmap on a functor should just map a function over the functor, nothing more. This behavior is described in the functor laws. There are two of them that all instances of Functor should abide by. They aren't enforced by Haskell automatically, so you have to test them out yourself.
-- The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor. If we write that a bit more formally, it means that fmap id = id
-- The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one. Formally written, that means that fmap (f . g) F = fmap f (fmap g F). When we write map ((^2) . digitToInt) xs it should be equal to map (^2) . map (digitToInt) xs

-- At first, the functor laws might seem a bit confusing and unnecessary, but then we see that if we know that a type obeys both laws, we can make certain assumptions about how it will act. If a type obeys the functor laws, we know that calling fmap on a value of that type will only map the function over it, nothing more. This leads to code that is more abstract and extensible, because we can use laws to reason about behaviors that any functor should have and make functions that operate reliably on any functor.


-- We can also look at functors as things that output values in a context. For instance, Just 3 outputs the value 3 in the context that it might or not output any values at all. [1,2,3] outputs three values—1, 2, and 3, the context is that there may be multiple values or no values. The function (+3) will output a value, depending on which parameter it is given.

-- If you think of functors as things that output values, you can think of mapping over functors as attaching a transformation to the output of the functor that changes the value. When we do fmap (+3) [1,2,3], we attach the transformation (+3) to the output of [1,2,3], so whenever we look at a number that the list outputs, (+3) will be applied to it. Another example is mapping over functions. When we do fmap (+3) (*3), we attach the transformation (+3) to the eventual output of (*3)



-------- Applicative functors --------

-- So far, when we were mapping functions over functors, we usually mapped functions that take only one parameter. But what happens when we map a function like *, which takes two parameters, over a functor?
--  Let's take a look at a couple of concrete examples of this. If we have Just 3 and we do fmap (*) (Just 3), what do we get? From the instance implementation of Maybe for Functor, we know that if it's a Just something value, it will apply the function to the something inside the Just. Therefore, doing fmap (*) (Just 3) results in Just ((*) 3), which can also be written as Just (* 3) if we use sections. Interesting! We get a function wrapped in a Just!

-- ghci> let a = fmap (*) [1,2,3,4]  
-- ghci> :t a  
-- a :: [Integer -> Integer]  

-- But what if we have a functor value of Just (3 *) and a functor value of Just 5 and we want to take out the function from Just (3 *) and map it over Just 5
-- Meet the Applicative typeclass. It lies in the Control.Applicative module and it defines two methods, pure and <*>

class (Functor f) => Applicative' f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

-- The first method it defines is called pure. Its type declaration is pure :: a -> f a. f plays the role of our applicative functor instance here. pure should take a value of any type and return an applicative functor with that value inside it.
-- <*> takes a functor that has a function in it and another functor and sort of extracts that function from the first functor and then maps it over the second one

-- So for Maybe, <*> extracts the function from the left value if it's a Just and maps it over the right value. If any of the parameters is Nothing, Nothing is the result.
instance Applicative' Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = Prelude.fmap f something

-- ghci> Just (+3) <*> Just 9  
-- Just 12  
-- ghci> pure (+3) <*> Just 10  
-- Just 13  
-- ghci> pure (+3) <*> Just 9  
-- Just 12  
-- ghci> Just (++"hahah") <*> Nothing  
-- Nothing  
-- ghci> Nothing <*> Just "woot"  
-- Nothing  

-- ghci> pure (+) <*> Just 3 <*> Just 5  
-- Just 8  
-- ghci> pure (+) <*> Just 3 <*> Nothing  
-- Nothing  
-- ghci> pure (+) <*> Nothing <*> Just 5  
-- Nothing  


-- Applicative functors and the applicative style of doing pure f <*> x <*> y <*> ... allow us to take a function that expects parameters that aren't necessarily wrapped in functors and use that function to operate on several values that are in functor contexts. The function can take as many parameters as we want, because it's always partially applied step by step between occurences of <*>

-- This becomes even more handy and apparent if we consider the fact that pure f <*> x equals fmap f x. Instead of writing pure f <*> x <*> y <*> ..., we can write fmap f x <*> y <*> .... This is why Control.Applicative exports a function called <$>, which is just fmap as an infix operator. Here's how it's defined:

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = Prelude.fmap f x  

-- By using <$>, the applicative style really shines, because now if we want to apply a function f between three applicative functors, we can write f <$> x <*> y <*> z
-- Let's take a closer look at how this works. We have a value of Just "johntra" and a value of Just "volta" and we want to join them into one String inside a Maybe functor. We do this:

-- ghci> (++) <$> Just "johntra" <*> Just "volta"  
-- Just "johntravolta"  

-- Anyway, when we do (++) <$> Just "johntra" <*> Just "volta", first (++), which has a type of (++) :: [a] -> [a] -> [a] gets mapped over Just "johntra", resulting in a value that's the same as Just ("johntra"++) and has a type of Maybe ([Char] -> [Char]). Notice how the first parameter of (++) got eaten up and how the as turned into Chars. And now Just ("johntra"++) <*> Just "volta" happens, which takes the function out of the Just and maps it over Just "volta", resulting in Just "johntravolta". Had any of the two values been Nothing, the result would have also been Nothing.



-- Lists (actually the list type constructor, []) are applicative functors. What a suprise! Here's how [] is an instance of Applicative:

instance Applicative' [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
-- [0,0,0,101,102,103,1,4,9]  
-- ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
-- [4,5,5,6,3,4,6,8]
-- ghci> (*) <$> [2,5,10] <*> [8,10,11]  
-- [16,20,22,40,50,55,80,100,110]  

-- It's easy to see how pure f <*> xs equals fmap f xs with lists. pure f is just [f] and [f] <*> xs will apply every function in the left list to every value in the right one, but there's just one function in the left list, so it's like mapping.


-- Another instance of Applicative that we've already encountered is IO. This is how the instance is implemented:
instance Applicative' IO where
    pure = return -- pure a = IO a
    a <*> b = do
        f <- a
        x <- b
        return (f x)

myAction1 :: IO String  
myAction1 = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  

myAction2 :: IO String  
myAction2 = (++) Prelude.<$> getLine Prelude.<*> getLine  




-- An instance of Applicative that we haven't encountered yet is ZipList, and it lives in Control.Applicative.

-- It turns out there are actually more ways for lists to be applicative functors. One way is the one we already covered, which says that calling <*> with a list of functions and a list of values results in a list which has all the possible combinations of applying functions from the left list to the values in the right list. If we do [(+3),(*2)] <*> [1,2], (+3) will be applied to both 1 and 2 and (*2) will also be applied to both 1 and 2, resulting in a list that has four elements, namely [4,5,2,4].
-- However, [(+3),(*2)] <*> [1,2] could also work in such a way that the first function in the left list gets applied to the first value in the right one, the second function gets applied to the second value, and so on. That would result in a list with two values, namely [4,4]. You could look at it as [1 + 3, 2 * 2].

-- ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
-- [101,102,103]  
-- ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
-- [101,102,103]  
-- ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
-- [5,3,3,4]  
-- ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  
-- [('d','c','r'),('o','a','a'),('g','t','t')]  



-- When we first talked about functors, we saw that they were a useful concept for values that can be mapped over. Then, we took that concept one step further by introducing applicative functors, which allow us to view values of certain data types as values with contexts and use normal functions on those values while preserving the meaning of those contexts.

-- Applicative functors are more powerful than just ordinary functors. With ordinary functors, we can just map functions over one functor. But with applicative functors, we can apply a function between several functors.




-------- The newtype keyword --------

-- So far, we've learned how to make our own algebraic data types by using the data keyword. We've also learned how to give existing types synonyms with the type keyword. In this section, we'll be taking a look at how to make new types out of existing data types by using the newtype keyword and why we'd want to do that in the first place.

-- Well, think about how we might write the data declaration for our ZipList a type. One way would be to do it like so:

-- data ZipList a = ZipList [a] 

-- A type that has just one value constructor and that value constructor has just one field that is a list of things. We might also want to use record syntax so that we automatically get a function that extracts a list from a ZipList

-- data ZipList a = ZipList { getZipList :: [a] }  

-- This looks fine and would actually work pretty well. We had two ways of making an existing type an instance of a type class, so we used the data keyword to just wrap that type into another type and made the other type an instance in the second way.

-- The newtype keyword in Haskell is made exactly for these cases when we want to just take one type and wrap it in something to present it as another type. In the actual libraries, ZipList a is defined like this:

-- newtype ZipList a = ZipList { getZipList :: [a] } 

-- newtype is usually faster than data. When using newtype, you're restricted to just one constructor with one field.

-- If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms. If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a newtype. And if you want to make something completely new, odds are good that you're looking for the data keyword.



-------- Monoids --------

-- * function and ++ function have some common properties:
--      The function takes two parameters.
--      The parameters and the returned value have the same type.
--      There exists such a value that doesn't change other values when used with the binary function. For example: value 1 in * function and value [] in ++ function because x * 1 = x and xs ++ [] = xs

-- A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function. When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value

-- There are a lot of other monoids to be found in the world of Haskell, which is why the Monoid type class exists. It's for types which can act like monoids. Let's see how the type class is defined:

class Monoid' m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr Main.mappend Main.mempty  

-- First of all, we see that only concrete types can be made instances of Monoid
-- The first function is mempty. It's not really a function, since it doesn't take parameters, so it's a polymorphic constant, kind of like minBound from Bounded. mempty represents the identity value for a particular monoid.
-- Next up, we have mappend, which, as you've probably guessed, is the binary function. It takes two values of the same type and returns a value of that type as well
-- The last function in this type class definition is mconcat. It takes a list of monoid values and reduces them to a single value by doing mappend between the list's elements.

-- Before moving on to specific instances of Monoid, let's take a brief look at the monoid laws.
--      mempty `mappend` x = x
--      x `mappend` mempty = x
--      (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)


-- Lists are monoids
instance Monoid' [a] where  
    mempty = []  
    mappend = (++)  

-- ghci> "pang" `mappend` mempty  
-- "pang"    
-- ghci> ("one" `mappend` "two") `mappend` "tree"  
-- "onetwotree"  
-- ghci> "one" `mappend` ("two" `mappend` "tree")  
-- "onetwotree"  
-- ghci> mconcat [[1,2],[3,6],[9]]  
-- [1,2,3,6,9] 
-- ghci> mempty :: [a]  
-- [] 


-- Product and Sum 
-- So there are two equally valid ways for numbers to be monoids, which way do choose? Well, we don't have to.  Remember, when there are several ways for some type to be an instance of the same type class, we can wrap that type in a newtype and then make the new type an instance of the type class in a different way.

newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Num a => Monoid' (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  

-- ghci> getProduct $ Product 3 `mappend` Product 9  
-- 27  
-- ghci> getProduct $ Product 3 `mappend` mempty  
-- 3 