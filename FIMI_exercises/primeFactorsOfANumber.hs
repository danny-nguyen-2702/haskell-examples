-- Write a function that generates factors for a given number.

-- The function takes an integer on the standard input and returns a list of integers 
-- (ObjC: array of NSNumbers representing integers). 
-- That list contains the prime factors in numerical sequence.

-- Examples
-- 1  ==>  []
-- 3  ==>  [3]
-- 8  ==>  [2, 2, 2]
-- 9  ==>  [3, 3]
-- 12 ==>  [2, 2, 3]


-- we don't need to use isPrime in this exercise but I just write here for future use
isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | otherwise = null . take 1 $ [x | x <- 2:[3,5..sqrtOfN], n `mod` x == 0]
    where sqrtOfN = floor . sqrt .fromIntegral $ n


findFactors :: Int -> [Int]
findFactors 1 = []
findFactors n =  factor : findFactors (n `div` factor)
    where factor = head [x | x <- [2..n], n `mod` x == 0]

-- test = all (all isPrime . findFactors) [20000..50000]

findFactors2 :: Int -> [(Int, Int)]
findFactors2 1 = []
findFactors2 n =  (factor, power) : findFactors2 (n `div` factor ^ power)
    where factor = head [x | x <- [2..n], n `mod` x == 0]
          power = last $ takeWhile (\p -> n `mod` factor ^ p == 0) [1..]