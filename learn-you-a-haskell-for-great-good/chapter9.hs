---------------------------------------- Input and Output ----------------------------------------

-- To run a program you can either compile it and then run the produced executable file by doing ghc --make chapter9 and then ./chapter9 or you can use the runhaskell command like so: runhaskell chapter9.hs and your program will be executed on the fly.

import Data.Char ( toUpper )
import Control.Monad ( when, forever, forM )   
import System.IO

main1 = do
    putStrLn "Enter your name here"
    name <- getLine
    putStrLn $ "Hmm you seem like a very kind person Mr." ++ name

-- We use do syntax to combine (or glue together) several I/O actions into one I/O action
-- putStrLn :: String -> IO () We can read the type of putStrLn like this: putStrLn takes a string and returns an I/O action that has a result type of () (i.e. the empty tuple, also know as unit)
-- An I/O action is something that, when performed, will carry out an action with a side-effect (that's usually either reading from the input or printing stuff to the screen) and will also contain some kind of return value inside it.
-- So, when will an I/O action be performed? Well, this is where main comes in. I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O action that we composed with a do block
-- getLine is an I/O action that contains a result type of String (getLine :: IO String). So you can read this piece of code "name <- getLine" like this: perform the I/O action getLine and then bind its result value to name. getLine has a type of IO String, so name will have a type of String.
-- And if we're taking data out of an I/O action, we can ONLY take it out when we're INSIDE another I/O action. If we want to deal with impure data, we have to do it in an impure environment. We use the binding operator <- to get the data inside an I/O action
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



-- some other functions that are useful when dealing with I/O

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



-- mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later
{-
ghci> mapM print [1,2,3]  
1  
2  
3  
[(),(),()] 

ghci> mapM_ print [1,2,3]  
1  
2  
3 
-}


-- forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever. It's located in Control.Monad. This little program will indefinitely ask the user for some input and spit it back to him, CAPSLOCKED
main7 = forever $ do  
    putStrLn "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  
    


-- forM (located in Control.Monad) is like mapM, only that it has its parameters switched around. The first parameter is the list and the second one is the function to map over that list, which is then sequenced.

main8 = do
    colors <- forM [1, 2, 3, 4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        getLine)
    
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM print colors
-- You can think of forM as meaning: make an I/O action for every element in this list. What each I/O action will do can depend on the element that was used to make the action. Finally, perform those actions and bind their results to something. We don't have to bind it, we can also just throw it away.


--------------------------------- Files and streams ---------------------------------

-- getContents is an I/O action that reads everything from the standard input until it encounters an end-of-file character. Its type is getContents :: IO String. What's cool about getContents is that it does lazy I/O. When we do foo <- getContents, it doesn't read all of the input at once, store it in memory and then bind it to foo. No, it's lazy! It'll say: "Yeah yeah, I'll read the input from the terminal later as we go along, when you really need it!".
-- Let's make program that takes some input and prints out only those lines that are shorter than 10 characters
main9 = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input = result
    where result = unlines shortLines
          shortLines = filter (\line -> length line < 10) allLines
          allLines = lines input
    

-- This pattern of getting some string from the input, transforming it with a function and then outputting that is so common that there exists a function which makes that even easier, called interact. interact takes a function of type String -> String as a parameter and returns an I/O action that will take some input, run that function on it and then print out the function's result.
main10 = interact shortLinesOnly


-- Just to show that this can be achieved in much less code (even though it will be less readable) and to demonstrate our function composition skill, we're going to rework that a bit further.
main11 = interact $ unlines . filter (\line -> length line < 10) . lines


-- Let's make a program that continuously reads a line and then tells us if the line is a palindrome or not. In our case, we have to replace each line of the input with either "palindrome" or "not a palindrome". So we have to write a function that transforms something like "elephant\nABCBA\nwhatever" into "not a palindrome\npalindrome\nnot a palindrome"
main12 = interact $ unlines . map (\str -> if str == reverse str then "palindrome" else "not a palindrome") . lines


-- One way to think about reading from the terminal is to imagine that it's like reading from a (somewhat special) file. Same goes for writing to the terminal, it's kind of like writing to a file. We can call these two files stdout and stdin, meaning standard output and standard input, respectively. Keeping that in mind, we'll see that writing to and reading from files is very much like writing to the standard output and reading from the standard input



-- We'll start off with a really simple program that opens a file called girlfriend.txt, which contains a verse from Avril Lavigne's #1 hit Girlfriend, and just prints out out to the terminal.

main13 = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- The type signature of openFile is openFile :: FilePath -> IOMode -> IO Handle. It states: openFile takes a file path and an IOMode and returns an I/O action that will open a file and have the file's associated handle encapsulated as its result. 
-- There are 4 values of IOMode: ReadMode | WriteMode | AppendMode | ReadWriteMode
-- A value of type Handle represents where our file is. We'll use that handle so we know which file to read from.



-- Another way of doing what we just did is to use the withFile function, which has a type signature of withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
main14 = withFile "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

-- withFile opens the file and then passes the handle to the function we gave it. It gets an I/O action back from that function and then makes an I/O action that's just like it, only it closes the file afterwards.
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' filePath ioMode f = do
    handle <- openFile filePath ioMode
    result <- f handle
    hClose handle
    return result


-- Just like we have hGetContents that works like getContents but for a specific file, there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc. They work just like their counterparts without the h, only they take a handle as a parameter and operate on that specific file instead of operating on standard input or standard output.

-- Loading files and then treating their contents as strings is so common that we have these three nice little functions to make our work even easier:

-- readFile function has a type signature of readFile :: FilePath -> IO String
main15 = do
    contents <- readFile "girlfriend.txt"
    putStr contents


-- writeFile has a type of writeFile :: FilePath -> String -> IO (). It takes a path to a file and a string to write to that file and returns an I/O action that will do the writing. If such a file already exists, it will be stomped down to zero length before being written on.
-- Here's how to turn girlfriend.txt into a CAPSLOCKED version and write it to girlfriendcaps.txt:
main16 = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" $ map toUpper contents


-- appendFile has a type signature that's just like writeFile, only appendFile doesn't truncate the file to zero length if it already exists but it appends stuff to it.
-- Let's say we have a file todo.txt that has one task per line that we have to do. Now let's make a program that takes a line from the standard input and adds that to our to-do list.
main = do
    task <- getLine
    appendFile "todo.txt" (task ++ "\n")


-- You can control how exactly buffering is done by using the hSetBuffering function. It takes a handle and a BufferMode and returns an I/O action that sets the buffering. BufferMode is a simple enumeration data type and the possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int). The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing, then the operating system determines the chunk size. NoBuffering means that it will be read one character at a time. NoBuffering usually sucks as a buffering mode because it has to access the disk so much. Reading files in bigger chunks can help if we want to minimize disk access or when our file is actually a slow network resource.

-- We can also use hFlush, which is a function that takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle. When we're doing line-buffering, the buffer is flushed after every line. When we're doing block-buffering, it's after we've read a chunk

