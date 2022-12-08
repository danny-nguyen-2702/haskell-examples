--------------------------------------- For a Few Monads More ---------------------------------------
import Control.Monad.Writer

-- We've seen how monads can be used to take values with contexts and apply them to functions and how using >>= or do notation allows us to focus on the values themselves while the context gets handled for us. 

main1 = do
    name <- getLine
    putStrLn name

main1' = getLine >>= putStrLn

-- In this chapter, we're going to learn about a few other monads



---------- Writer monad ----------
-- the Writer monad is for values that have another value attached that acts as a sort of log value.
-- For instance, we might want to equip our values with strings that explain what's going on. Instead of just giving us a True or False value, we want it to also return a log string that says what it did

isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  

-- So far so good. isBigGang takes a normal value and returns a value with a context. Now what if we already have a value that has a log string attached to it, such as (3, "Smallish gang."), and we want to feed it to isBigGang? It seems like once again, we're faced with this question: if we have a function that takes a normal value and returns a value with a context, how do we take a value with a context and feed it to the function?

-- let's make a function that takes a value with an attached log, that is, an (a,String) value and a function of type a -> (b,String) and feeds that value into the function

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (a, log) f = let (y, newLog) = f a in (y, log ++ ", " ++ newLog)
-- ghci> applyLog (3, "Initialize value") (\x -> (x+3, "Added 3"))
-- (6,"Initialize value, Added 3")


-- now we will change the applyLog so the log could be bytestrings. Because both lists and bytestrings are monoids, which means that they implement the mappend function
applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (a, log) f = let (y, newLog) = f a in (y, log `mappend` newLog)


-- The Writer type
{-
newtype Writer w a = Writer { runWriter :: (a, w) }  

instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
-}

-- First off, let's examine >>=. Its implementation is essentially the same as applyLog, only now that our tuple is wrapped in the Writer newtype, we have to unwrap it when pattern matching. We take the value x and apply the function f to it. This gives us a Writer w a value and we use a let expression to pattern match on it. We present y as the new result and use mappend to combine the old monoid value with the new one. We pack that up with the result value in a tuple and then wrap that with the Writer constructor so that our result is a Writer value instead of just an unwrapped tuple.
-- So, what about return? It has to take a value and put it in a default minimal context that still presents that value as the result. So what would such a context be for Writer values? If we want the accompanying monoid value to affect other monoid values as little as possible, it makes sense to use mempty. mempty is used to present identity monoid values, such as "" and Sum 0 and empty bytestrings. Whenever we use mappend between mempty and some other monoid value, the result is that other monoid value. So if we use return to make a Writer value and then use >>= to feed that value to a function, the resulting monoid value will be only what the function returns.


logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

-- ghci> multWithLog
-- WriterT (Identity (15,["Got number: 3","Got number: 5"]))


-- Sometimes we just want some monoid value to be included at some particular point. For this, the tell function is useful
multWithLog' :: Writer [String] Int
multWithLog' = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

-- ghci> multWithLog'
-- WriterT (Identity (15,["Got number: 3","Got number: 5","Gonna multiply these two"]))


---------- Adding logging to programs ----------

-- Euclid's algorithm is an algorithm that takes two numbers and computes their greatest common divisor (gcd). That is, the biggest number that still divides both of them. Here's the normal algorithm:

gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

-- Now, we want to equip our result with a context, and the context will be a monoid value that acts as a log.

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)
-- This function takes two normal Int values and returns a Writer [String] Int, that is, an Int that has a log context. In the case where b is 0, instead of just giving a as the result, we use a do expression to put together a Writer value as a result. First we use tell to report that we're finished and then we use return to present a as the result of the do expression.
-- Next, we have the case when b isn't 0. In this case, we log that we're using mod to figure out the remainder of dividing a and b. Then, the second line of the do expression just recursively calls gcd'. Remember, gcd' now ultimately returns a Writer value, so it's perfectly valid that gcd' b (a `mod` b) is a line in a do expression.

-- ghci> gcd'' 12 5
-- WriterT (Identity (1,["12 mod 5 = 2","5 mod 2 = 1","2 mod 1 = 0","Finished with 1"]))
-- ghci> fst $ runWriter (gcd'' 12 5)
-- 1


---------- Error error on the wall ----------

-- We know by now that Maybe is used to add a context of possible failure to values. A value can be a Just something or a Nothing. However useful it may be, when we have a Nothing, all we know is that there was some sort of failure, but there's no way to cram some more info in there telling us what kind of failure it was or why it failed.
-- The Either e a type on the other hand, allows us to incorporate a context of possible failure to our values while also being able to attach values to the failure, so that they can describe what went wrong or provide some other useful info regarding the failure.
-- This is pretty much just an enhanced Maybe, so it makes sense for it to be a monad, because it can also be viewed as a value with an added context of possible failure, only now there's a value attached when there's an error as well.

{-
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
-}

-- ghci> :t strMsg  
-- strMsg :: (Error a) => String -> a  
-- ghci> strMsg "boom!" :: String  
-- "boom!"  

-- ghci> Left "boom" >>= \x -> return (x+1)  
-- Left "boom"  
-- ghci> Right 100 >>= \x -> Left "no way!"  
-- Left "no way!"  

-- ghci> Right 3 >>= \x -> return (x + 100)  
-- Right 103


---------- filterM ----------
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  

-- ghci> runWriter $ filterM keepSmall [9,1,5,2,10,3] 
-- ([1,2,3],["9 is too large, throwing it away","Keeping 1","5 is too large, throwing it away","Keeping 2","10 is too 
-- large, throwing it away","Keeping 3"])

-- ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- [1, 2, 3]


---------- foldM ----------

binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  

-- ghci> foldM binSmalls 0 [2,8,3,1]  
-- Just 14  
-- ghci> foldM binSmalls 0 [2,11,3,1]  
-- Nothing  