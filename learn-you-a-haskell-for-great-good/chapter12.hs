--------------------------------- A Fistful of Monads ---------------------------------


-- When we have a normal value `a` and a normal function `a -> b` it's really easy to feed the value to the function — you just apply the function to the value normally and that's it. But if you have a value with a context, `m a`, how do you apply to it a function that takes a normal `a` and returns a value with a context? That is, how do you apply a function of type `a -> m b` to a value of type `m a`?

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  

-- if we have a function that takes a normal value and returns a value with a context, how do we take a value with a context and feed it to the function?

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

-- ghci> applyMaybe (Just 3) (\x -> Just (x + 5))
-- Just 8

class Monad' m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x Main.>>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg  

-- The first function that the Monad type class defines is return. It's the same as pure, only with a different name. Its type is (Monad m) => a -> m a. It takes a value and puts it in a minimal default context that still holds that value.
-- The next function is >>=, or bind. It's like function application, only instead of taking a normal value and feeding it to a normal function, it takes a monadic value (that is, a value with a context) and feeds it to a function that takes a normal value but returns a monadic value.
-- Next up, we have >>. We won't pay too much attention to it for now because it comes with a default implementation and we pretty much never implement it when making Monad instances.
-- The final function of the Monad type class is fail. We never use it explicitly in our code. Instead, it's used by Haskell to enable failure in a special syntactic construct for monads that we'll meet later.


instance Monad' Maybe where
    return = Just
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

-- ghci> return "WHAT" :: Maybe String  
-- Just "WHAT"  
-- ghci> Just 9 >>= \x -> return (x*10)  
-- Just 90  
-- ghci> Nothing >>= \x -> return (x*10)  
-- Nothing 



type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
    | abs(left + n - right) > 3 = Nothing
    | otherwise = Just (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) 
    | abs (left - (right + n)) > 3 = Nothing
    | otherwise = Just (left, right + n)

-- Remember, landLeft 2 has a type of Pole -> Maybe Pole. We couldn't just feed it the Maybe Pole that is the result of landRight 1 (0,0), so we use >>= to take that value with a context and give it to landLeft 2.
-- ghci> landRight 1 (0,0) >>= landLeft 2 
-- Just (2,1)  
-- ghci> Nothing >>= landLeft 2  
-- Nothing  


-- With this, we can now chain landings that may fail because >>= allows us to feed a monadic value to a function that takes a normal one.
-- ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
-- Just (2,4)  
-- ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
-- Nothing  



-------------- do notation --------------

-- It's important to remember that do expressions are just different syntax for chaining monadic values.
-- In a do expression, every line is a monadic value. To inspect its result, we use <-
-- The last monadic value in a do expression can't be used with <- to bind its result
foo :: Maybe String  
foo = Just 3   Main.>>= (\x -> 
      Just "!" Main.>>= (\y -> 
      Just (show x ++ y)))  

foo' :: Maybe String  
foo' = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  

marySue :: Maybe Bool
marySue = Just 9 Main.>>= (\x -> Just (x > 8))


marySue' :: Maybe Bool
marySue' = do
    x <- Just 9
    Just (x > 8)



-- In do notation, when we bind monadic values to names, we can utilize pattern matching
-- When pattern matching fails in a do expression, the fail function is called. It's part of the Monad type class and it enables failed pattern matching to result in a failure in the context of the current monad instead of making our program crash.
justH :: Maybe Char
justH = do
    (x:xs) <- Just "Hello"
    Prelude.return x



------------ The list monad ------------

-- instance Monad' [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     Main.fail _ = []  

-- ghci> [3,4,5] >>= \x -> [x,-x]  
-- [3,-3,4,-4,5,-5]  

example1 = [1,2] Prelude.>>= \n -> ['a','b'] Prelude.>>= \ch -> Prelude.return (n,ch)  

example1' = do
    n <- [1, 2]
    ch <- ['a', 'b']
    Prelude.return (n, ch)

example1'' = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  




------------ A knight's quest ------------

-- Here's a problem that really lends itself to being solved with non-determinism. Say you have a chess board and only one knight piece on it. We want to find out if the knight can reach a certain position in three moves. We'll just use a pair of numbers to represent the knight's position on the chess board. The first number will determine the column he's in and the second number will determine the row.

-- Let's make a type synonym for the knight's current position on the chess board:
type KnightPos = (Int,Int)  

-- moveKnight :: KnightPos -> [KnightPos]  
-- moveKnight (c,r) = do  
--     (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
--                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
--                ]  
--     Preulude.guard (c' `elem` [1..8] && r' `elem` [1..8])  
--     Prelude.return (c',r') 


-- Here's a function that takes the knight's position and returns all of its next moves:
moveKnight' :: KnightPos -> [KnightPos]  
moveKnight' (c, r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  


in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight' start  
    second <- moveKnight' first  
    moveKnight' second  

in3' start = Prelude.return start Prelude.>>= moveKnight' Prelude.>>= moveKnight' Prelude.>>= moveKnight'  

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start


-------------- Monad laws --------------

-- Haskell allows any type to be an instance of any type class as long as the types check out. It can't check if the monad laws hold for a type though, so if we're making a new instance of the Monad type class, we have to be reasonably sure that all is well with the monad laws for that type.


-- The first monad law states that if we take a value, put it in a default context with return and then feed it to a function by using >>=, it's the same as just taking the value and applying the function to it.
-- return x >>= f is the same damn thing as f x
-- ghci> return "WoM" >>= (\x -> [x,x,x])  
-- ["WoM","WoM","WoM"] 

-- The second law states that if we have a monadic value and we use >>= to feed it to return, the result is our original monadic value. Formally:
-- m >>= return is no different than just m
-- ghci> Just "move on up" >>= (\x -> return x)  
-- Just "move on up"


-- The final monad law says that when we have a chain of monadic function applications with >>=, it shouldn't matter how they're nested. Formally written:
-- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)