module Cipher where

import Data.Char

-- A Caesar cipher is a simple substitution cipher, in which each letter is replaced by the letter that is a fixed number of places down the alphabet from it.

-- You should include an unCaesar function that will decipher your text as well. In a later chapter, we will test it.

shiftRightward :: Int -> Char -> Char
shiftRightward n c
    | isLower c = chr ((n + ord c - 97) `mod` 26 + 97)
    | isUpper c = chr ((n + ord c - 65) `mod` 26 + 65)
    | otherwise = c

caesar :: Int -> String -> String
caesar n = map (shiftRightward n)

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n) 