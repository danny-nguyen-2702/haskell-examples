module Main (main) where

import Control.Monad (forever) 
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

-- Read all the words in the dict.txt file and put into WordList
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 6

-- Filtered out words that doesn't match length requirements and wrap the word list inside IO
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in l >= minWordLength && l <= maxWordLength

-- Pick a random word from a word list
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- We start with a datatype for our puzzle. The puzzle is a product of a String, a list of Maybe Char, and a list of Char. 
data Puzzle = Puzzle String [Maybe Char] [Char]


-- Next we’re going to write an instance of the typeclass Show for our datatype Puzzle.
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
  

-- First we’re going to write a function that will take our puzzle word and turn it into a list of Nothing
freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

-- Now we need a function that looks at the Puzzle String and determines whether the character you guessed is an element of that string.
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w


-- The next function is to check and see if it is an element of the guessed list.
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar = 
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]

  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)
    

-- All right, next we need to devise a way to stop the game after a certain number of guesses, only incorrect guesses count towards the guess limit.
gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filledInSoFar guessed) = 
  if countGuessesLeft 15 p == 0 then do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  else putStrLn $ "You have " ++ show (countGuessesLeft 15 p) ++ " guess(es) left"


countGuessesLeft :: Int -> Puzzle -> Int
countGuessesLeft limit (Puzzle _ filledInSoFar guessed) = limit - numberOfGuessed
  where numberOfGuessed = length guessed - sum (map (\x -> if isJust x then 1 else 0) filledInSoFar)


gameWin :: Puzzle -> IO ()
gameWin (Puzzle w filledInSoFar _) =
  if all isJust filledInSoFar then do
    putStrLn $ "You win! The word is " ++ w
    exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  
  putStr "Guess a letter: "
  guess <- getLine

  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "You guess must be a single character"



main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


