---------------------------- RECURSION ----------------------------
-- Definition: Recursion is actually a way of defining functions in which the function is applied inside its own definition.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x


take' :: Int -> [a] -> [a]
take' n xs
    | n <= 0 = []
    | null xs = []
    | otherwise = head xs : take' (n-1) (tail xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x : repeat' x


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys


-- a sorted list is a list that has all the values smaller than (or equal to) the head of the list in front (and those values are sorted), then comes the head of the list in the middle and then come all the values that are bigger than the head (they're also sorted)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smallers ++ [x] ++ quicksort biggers
    where smallers = [y | y <- xs, y <= x]
          biggers = [z | z <- xs, z > x]