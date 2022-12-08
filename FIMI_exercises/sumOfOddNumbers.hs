{-
Given the triangle of consecutive odd numbers:

             1
          3     5
       7     9    11
   13    15    17    19
21    23    25    27    29
...
Calculate the sum of the numbers in the nth row of this triangle (starting at index 1) e.g.: (Input --> Output)

ghci> calculate 1 -->  1
ghci> calculate 2 --> 3 + 5 = 8
-}

calculate :: Int -> Int
calculate n = sum $ take n [maxRowN, maxRowN - 2 .. 1]
    where maxRowN = n * (n+1) - 1