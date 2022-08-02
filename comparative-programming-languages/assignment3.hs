----------------------------- Basic Functions -----------------------------

------------ Task 1 ------------
-- The snoc x lst function returns a new list that is the same as lst, except x has been added to the end of it. It has this signature: snoc :: a -> [a] -> [a]
-- For example, snoc 5 [1,2,3] is [1,2,3,5], and snoc 's' "cat" is "cats"
-- Implement snoc using only basic recursion. Do not use ++ or reverse or any other such high-level functions.

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : (snoc x ys)


------------ Task 2 ------------
-- Write your own version of the Haskell append operator ++ with this signature:
-- myappend :: [a] -> [a] -> [a]
-- Of course, don’t use functions like ++ or concat in your answer.

myappend :: [a] -> [a] -> [a]
myappend [] ys = ys
myappend (x:xs) ys = x : myappend xs ys


------------ Task 3 ------------
-- Write your own version of reverse with this signature:
-- myreverse :: [a] -> [a]
-- Don’t use any non-trivial functions in your answer unless you write those functions yourself.

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]


------------ Task 4 ------------
-- Write a function called count_emirps n that returns the number of emirps less than, or equal to, n.
-- An emirp is a prime number that is a different prime when its digits are reversed. For example, 107 is an emirp because 107 is prime, and its reverse, 701, is a different prime. However, 7 and 101 are not emirps because while their reverses are primes, they are not different primes.
-- The first few emirps are: 13, 17, 31, 37, 71, 73, ...
-- count_emirps has this signature: count_emirps :: Int -> Int
-- For example, count_emirps 100 returns 8, and count_emirps 1000 returns 36. If n is less than 13, count_emirps n returns 0.

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = foldl (\acc x -> acc && (n `mod` x /= 0)) True [2..n `div` 2]

isEmirp :: Int -> Bool
isEmirp n = n /= reversedN && isPrime n && isPrime reversedN
    where reversedN = read (reverse (show n)) :: Int

count_emirps :: Int -> Int
count_emirps n = length $ filter isEmirp [1..n]


------------ Task 5 ------------
-- Write a function called biggest_sum that takes a list of one, or more, integer lists as input, and returns the list with the greatest sum. It has this signature:
-- biggest_sum :: [[Int]] -> [Int]
-- For example, biggest_sum [[2,5], [-1,3,4], [2]] returns [2,5].
-- You can assume the list passed to biggest_sum is non-empty. If one, or more more, lists are tied for the biggest sum, then return the first one.

biggest_sum :: [[Int]] -> [Int]
biggest_sum = foldl (\acc x -> if sum x > sum acc then x else acc) []


------------ Task 6 ------------
-- Write a function called greatest, which has the following signature:
-- greatest :: (a -> Int) -> [a] -> a
-- greatest f seq returns the item in seq that maximizes function f. For example:
-- > greatest sum [[2,5], [-1,3,4], [2]]
-- [2,5]
-- > greatest length ["the", "quick", "brown", "fox"]
-- "quick"
-- > greatest id [51,32,3]
-- 51
-- If more than one item maximizes f, then greatest f returns the first one.

greatest :: (a -> Int) -> [a] -> a
greatest f [] = error "Empty list!"
greatest f xs = foldl1 (\acc x -> if f x > f acc then x else acc) xs -- foldl1 is a variant of foldl that has no base case, and thus may only be applied to non-empty structures.





----------------------------- Basic Bits -----------------------------

------------ Task 7 ------------
-- Write a function called is_bit x that returns True when x is 0 or 1, and False otherwise.
-- Assume x is of type Int, and the type of the returned value is Bool.
-- Include the most general type signature.

is_bit :: (Eq a, Num a) => a -> Bool
is_bit 0 = True
is_bit 1 = True
is_bit _ = False


------------ Task 8 ------------
-- Write a function called flip_bit x that returns 1 if x is 0, and 0 if x is 1. If x is not a bit, then call error msg, where msg is a helpful error message string.
-- Assume x is of type Int, and the type of the returned value is also Int.
-- Include the most general type signature.

flip_bit :: (Eq a, Num a) => a -> a
flip_bit 1 = 0
flip_bit 0 = 1
flip_bit _ = error "Input is not a bit"


------------ Task 9 ------------
-- In each of the following functions, x is a list of Int values, and the returned value has type Bool. Include the most general type signature for each function.
----- a. Write a function called is_bit_seq1 x that returns True if x is the empty list, or if it contains only bits (as determined by is_bit). It should return False otherwise. Use recursion and guarded commands in your solution.
----- b. Re-do the previous question, except this time name the function is_bit_seq2 x, and use recursion and at least one if-then-else expression in your solution. Don’t use any guarded commands.
----- c. Re-do the previous question, except this time name the function is_bit_seq3 x, and don’t use recursion, guarded commands, or if- then-else in your solution. Instead, use a higher-order function to calculate the answer in one expression.

is_bit_seq1 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq1 xs
    | null xs = True
    | otherwise = is_bit (head xs) && is_bit_seq1 (tail xs)

is_bit_seq2 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq2 xs = if null xs
                 then True
                 else is_bit (head xs) && is_bit_seq1 (tail xs)

is_bit_seq3 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq3 = foldr (\x acc -> acc && is_bit x) True


------------ Task 10 ------------
-- In each of the following functions, x is a list of Int values, and the type of the returned value is also a list of Int. Include the most general type signature for each function.
---- a. Write a function called invert_bits1 x that returns a sequence of bits that is the same as x, except 0s become 1s and 1s become 0s. For example, invert_bits1 [0,1,1,0] returns [1,0,0,1]. Use basic recursion in your solution.
---- b. Re-do the previous question, but name the function invert_bits2 x, and implement it using the map function (and no recursion).
---- c. Re-do the previous question, but name the function invert_bits3 x, and implement it using a list comprehension (and no recursion, and no map function).

invert_bits1 :: (Eq a, Num a) => [a] -> [a]
invert_bits1 [] = []
invert_bits1 (x:xs) = flip_bit x : invert_bits1 xs

invert_bits2 :: (Eq a, Num a) => [a] -> [a]
invert_bits2 = map flip_bit

invert_bits3 :: (Eq a, Num a) => [a] -> [a]
invert_bits3 xs = [flip_bit x | x <- xs]

------------ Task 11 ------------
-- Write a function called bit_count x that returns a pair of values indicating the number of 0s and 1s in x. For example, bit_count [1,1,0,1] returns the pair (1, 3), meaning there is one 0 and three 1s in the list.
-- Assume x is a list of Int values, and only contains bits. The type of the returned value is (Int, Int). Include the most general type signature.

bit_count :: (Eq a, Num a) => [a] -> (Int, Int)
bit_count = foldr (\x (numberOf0s, numberOf1s) -> if x == 0 then (numberOf0s+1, numberOf1s) else (numberOf0s, numberOf1s+1)) (0, 0)


------------ Task 12 ------------
-- Write a function called all_basic_bit_seqs n that returns a list of all bit sequences of length n. The order of the sequences doesn’t matter. If n is less than 1, then return an empty list.
-- Assume n is an Int, and the returned value is a list of Int lists. Include the most general type signature.


-- My createBitSequence function create a bit sequence in form of a list that represents the decimal number equal to val variable and has a length equals the length variable. 
-- for example: createBitSequence 5 4 will return [0,1,0,1] because 0101 has a length of 4 bits and equals to 5 in decimal number. Similarly, createBitSequence 5 6 will return [0,0,0,1,0,1] and createBitSequence 4 4 will return [0,1,0,0]
createBitSequence :: (Ord a, Eq a, Integral a) => a -> a -> [a]
createBitSequence _ 0 = []
createBitSequence val length
    | val >= valueAtCurrentPosition = 1 : createBitSequence (val - valueAtCurrentPosition) (length - 1)
    | otherwise = 0 : createBitSequence val (length - 1)
    where valueAtCurrentPosition = round (2 ** fromIntegral (length-1))


all_basic_bit_seqs :: (Ord a, Integral  a) => a -> [[a]]
all_basic_bit_seqs n = map (\x -> createBitSequence x n) [0..round (2 ** fromIntegral n - 1)]



----------------------------- A Custom List Data Type -----------------------------

-- Haskell has good built-in support for lists that you should use for most programs. In this question we will implement our own list type as follows:

data List a = Empty | Cons a (List a) deriving Show

{-
This is an algebraic data type. It defines a type called List a, that represents a list of 0, or more, values of type a. A List a is either Empty, or it is of the form (Cons first rest), where first is of type a, and rest is of type List a. This mimics the common “cons cell” implementation of a list in a language like Lisp.

The line deriving Show is added as a convenience: it lets Haskell convert a List a to a string for printing.

Try to complete the following tasks with the above custom list data type
-}

------------ Task 13 ------------
-- Implement toList :: [a] -> List a, which converts a regular Haskell list to a List a. For example:
-- ghci> toList [] returns Empty
-- ghci> toList [2, 7, 4] Cons 2 (Cons 7 (Cons 4 Empty))

toList :: [a] -> List a
toList = foldr Cons Empty -- foldr (\x acc -> Cons x acc) Empty

------------ Task 14 ------------
-- Implement toHaskellList :: List a -> [a], which converts a List a to a regular Haskell list. For example:
-- ghci> toHaskellList Empty returns []
-- ghci> toHaskellList (Cons 2 (Cons 7 (Cons 4 Empty))) returns [2,7,4]
    
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x xs) = x : toHaskellList xs



-- For the following questions, don’t use toList or toHaskellList in your implementations. Only use them for testing and debugging. Stick to basic recursion and Haskell prelude functions for your solution code.

------------ Task 15 ------------
-- Implement append A B, that returns a new List a that consists of all the elements of A followed by all the elements of B. In other words, it does for List a what ++ does for regular Haskell lists. For example

append :: List a -> List a -> List a
append Empty ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

------------ Task 16 ------------
-- Implement the function removeAll f L that returns a List a that is the same as L but all items satisfying f (i.e. for which f returns True) have been removed. f is a predicate function of type a -> Bool and L has type List a. For example:
-- ghci> removeAll even Empty return Empty
-- ghci> removeAll (\x -> x == 'b') (Cons 'b' (Cons 'u' (Cons 'b' Empty))) returns Cons 'u' Empty
-- ghci> removeAll even (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))) returns Cons 1 (Cons 3 Empty)

removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons x xs)
    | f x = removeAll f xs
    | otherwise = Cons x (removeAll f xs)

------------ Task 17 ------------
-- Implement sort L, where L has type List a, that returns a new List a that is a sorted version of L (in ascending order). Use either quicksort or mergesort. It must have this type signature: sort :: Ord a => List a -> List a
-- For example:
-- ghci> sort Empty returns Empty
-- ghci> sort (Cons 'c' (Cons 'a' (Cons 'r' (Cons 't' Empty)))) returns Cons 'a' (Cons 'c' (Cons 'r' (Cons 't' Empty)))

sort :: Ord a => List a -> List a
sort Empty = Empty
sort (Cons x xs) = smallers `append` (Cons x Empty) `append` biggers
    where smallers = removeAll (>x) xs
          biggers = removeAll (<=x) xs
