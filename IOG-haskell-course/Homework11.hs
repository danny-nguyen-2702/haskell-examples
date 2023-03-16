import Data.List
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, listDirectory)

{-
We imported some functions that you'll need to complete the homework.
FilePath is just a synonym for String. Although, make sure to follow the standard path
representation when using them (https://en.wikipedia.org/wiki/Path_(computing).
getCPUTime    :: IO Integer
doesFileExist :: FilePath -> IO Bool
listDirectory :: FilePath -> IO [FilePath]
You can hover over the functions to know what they do.
-}

{-
-- Question 1 --
Define an IO action that counts the number of files in the current directory
and prints it to the terminal inside a string message.
-}

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles [] = return []
filterFiles (x:xs) = do
  isFile <- doesFileExist x
  if isFile
    then do
      files <- filterFiles xs
      return $ x : files
    else filterFiles xs

listFiles :: IO ()
listFiles = do
  filePaths <- listDirectory "."
  filesOnly <- filterFiles filePaths
  putStrLn $ "Number of files in current directory: " ++ show (length filesOnly)


{-
-- Question 2 --
Write an IO action that asks the user to type something, then writes the message
to a file called msg.txt, and after that, it reads the text from the msg.txt
file and prints it back. Use the writeFile and readFile functions.
-}

createMsg :: IO ()
createMsg = do
  putStrLn "Type something here"
  userTxt <- getLine
  writeFile "msg.txt" userTxt
  message <- readFile "msg.txt"
  putStrLn message


{-
-- Context for Questions 3 and 4 --
In cryptography, prime numbers (positive integers only divisible by themselves and 1) play a fundamental
role in providing unbreakable mathematical structures. These structures, in turn, are leveraged to
establish secure encryption.
But, generating primes is a computational straining problem, as we will measure in the following exercise.
This is because, to know whether a number is a prime number, you first need to know all the previous primes
and then check that they are not a divisor of this number. So, this problem gets bigger and bigger!
Our lead cryptographer provided us with 3 different algorithms (primes1, primes2, and primes3). All three
correctly produce a list of all the prime numbers until a limit (that we provide as a parameter).
Our job is not to understand these algorithms but to measure which is the fastest and print the largest
prime number below our limit. Do it step by step, starting with question 3.
-}

primes1 :: Integer -> [Integer]
primes1 m = sieve [2 .. m]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2 .. m]
 where
  sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
  sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2 .. m]
 where
  turner [] = []
  turner (p : xs) = p : turner [x | x <- xs, x < p * p || rem x p /= 0]

{-
-- Question 3 --
Define an IO action that takes an IO action as input and calculates the time it takes to execute.
Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
The CPU time here is given in picoseconds (which is 1/1000000000000th of a second).
-}

timeIO :: IO a -> IO ()
timeIO action = do
  startTime <- getCPUTime
  action
  endTime <- getCPUTime
  print $ endTime - startTime



{-
-- Question 4 --
Write an action that retrieves a value from the standard input, parses it as an integer,
and compares the time all three algorithms take to produce the largest prime before the
limit. Print the number and time to the standard output.
-}

benchmark :: IO ()
benchmark = do
  putStrLn "Enter the limit number"
  input <- getLine
  let limit = read input :: Integer

  putStrLn "These are the largest prime and execution time of each algorithm"
  resultAndTime primes1 limit
  resultAndTime primes2 limit
  resultAndTime primes3 limit

  where
    resultAndTime f n = timeIO $ print $ last (f n)


{-
 -- Question 5 -- EXTRA CREDITS -- (In case the previous ones were too easy)
Write a program that prints the directory tree structure from the current folder.
Below you can see an example output of how such a structure looks like:
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: You can use the function doesFileExist, which takes in a FilePath and returns
True if the argument file exists and is not a directory, and False otherwise.
-}

drawTree :: [FilePath] -> FilePath -> IO ()
drawTree []     parentPath = return ()
drawTree (x:xs) parentPath = do
  let currentHeight = length $ filter (== '/') parentPath
      prefixCharacter = if null xs then "└── " else "├── " -- if x is the last file/folder of the current directory, use └── character
      indentation = case currentHeight of
                      0 -> ""
                      n -> replicate (4*(n-1)) ' ' ++ prefixCharacter

  isFile <- doesFileExist $ parentPath ++ x
  if isFile
    then putStrLn $ indentation ++ x -- if x is file, print it name
    else do
      putStrLn $ indentation ++ x -- if x is folder, print it name and recursively print its children
      subFilesAndFolders <- listDirectory $ parentPath ++ x ++ "/"
      drawTree subFilesAndFolders (parentPath ++ x ++ "/")

  drawTree xs parentPath -- recursively print the remaining files and folders under the same directory


drawCurrentDirectoryTree :: IO ()
drawCurrentDirectoryTree = drawTree ["."] ""
