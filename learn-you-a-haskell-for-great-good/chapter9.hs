---------------------------------------- Input and Output ----------------------------------------

-- To run a program you can either compile it and then run the produced executable file by doing ghc --make chapter9 and then ./chapter9 or you can use the runhaskell command like so: runhaskell chapter9.hs and your program will be executed on the fly.

import Data.Char ( toUpper )
import Control.Monad ( when, forever )   

main1 = do
    putStrLn "Enter your name here"
    name <- getLine
    putStrLn $ "Hmm you seem like a very kind person Mr." ++ name

-- We can read the type of putStrLn like this: putStrLn takes a string and returns an I/O action that has a result type of ()
-- An I/O action is something that, when performed, will carry out an action with a side-effect (that's usually either reading from the input or printing stuff to the screen) and will also contain some kind of return value inside it.
-- So, when will an I/O action be performed? Well, this is where main comes in. I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O action that we composed with a do block
-- getLine is an I/O action that contains a result type of String (getLine :: IO String). So you can read this piece of code "name <- getLine" like this: perform the I/O action getLine and then bind its result value to name. getLine has a type of IO String, so name will have a type of String.
-- And if we're taking data out of an I/O action, we can ONLY take it out when we're INSIDE another I/O action. If we want to deal with impure data, we have to do it in an impure environment.
-- In a do block, the last action cannot be bound to a name because the do block automatically extracts the value from the last action and binds it to its own result


main2 = do
    putStrLn "Enter your first name"
    firstName <- getLine
    putStrLn "Enter your last name"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"


-- Now we're going to make a program that continuously reads a line and prints out the same line with the words reversed. The program's execution will stop when we input a blank line
main3 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main3

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- Let's first take a look at what happens under the else clause. Because, we have to have exactly one I/O action after the else, we use a do block to glue together two I/O actions into one
-- Now let's look at the return () part in the if statement. In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value. If you think about the box analogy from before, it takes a value and wraps it up in a box. The resulting I/O action doesn't actually do anything, it just has that value encapsulated as its result. So in an I/O context, return "haha" will have a type of IO String. Because we needed some I/O action to carry out in the case of an empty input line, so we just made a bogus I/O action that doesn't do anything by writing return ().


-- Using return doesn't cause the I/O do block to end in execution or anything like that. All these returns do is that they make I/O actions that don't really do anything except have an encapsulated result. We can use return in combination with <- to bind stuff to names
main4 = do
    name <- return "haha"
    putStrLn name
-- So you see, return is sort of the opposite to <-. While return takes a value and wraps it up in a box, <- takes a box (and performs it) and takes the value out of it, binding it to a name.
-- When dealing with I/O do blocks, we mostly use return either because we need to create an I/O action that doesn't do anything or because we don't want the I/O action that's made up from a do block to have the result value of its last action, but we want it to have a different result value, so we use return to make an I/O action that always has our desired result contained and we put it at the end.



-- some other that are useful when dealing with I/O

-- putStr is much like putStrLn in that it takes a string as a parameter and returns an I/O action that will print that string to the terminal, only putStr doesn't jump into a new line after printing out the string while putStrLn does

-- putChar takes a character and returns an I/O action that will print it out to the terminal. 

-- putStr is actually defined recursively with the help of putChar. The edge condition of putStr is the empty string, so if we're printing an empty string, just return an I/O action that does nothing by using return (). If it's not empty, then print the first character of the string by doing putChar and then print of them using putStr
putStr' :: String -> IO ()  
putStr' [] = return ()  
putStr' (x:xs) = do  
    putChar x  
    putStr' xs  


-- the print function takes a value of any type that's an instance of Show (meaning that we know how to represent it as a string), calls show with that value to stringify it and then outputs that string to the terminal. Basically, it's just putStrLn . show

-- getChar is an I/O action that reads a character from the input


-- The when function is found in Control.Monad (to get access to it, do import Control.Monad). It's interesting because in a do block it looks like a control flow statement, but it's actually a normal function. It takes a boolean value and an I/O action if that boolean value is True, it returns the same I/O action that we supplied to it. However, if it's False, it returns the return (). So as you can see, it's useful for encapsulating the if something then do some I/O action else return () pattern.
main5 = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main5 
    

-- sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other. The result contained in that I/O action will be a list of the results of all the I/O actions that were performed. Its type signature is sequence :: [IO a] -> IO [a]
main6 = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs


-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever. It's located in Control.Monad. This little program will indefinitely ask the user for some input and spit it back to him, CAPSLOCKED
main = forever $ do  
    putStrLn "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  
    
