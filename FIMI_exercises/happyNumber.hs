-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True

import Data.Char ( digitToInt )

isHappy :: Int -> Bool
isHappy n = check [n]
    where check all@(x:xs)
                | x == 1 = True
                | x `elem` xs = False
                | otherwise = check $ calculate x : all
          calculate = sum . map ((^ 2) . digitToInt) . show


        -- | (sum . map ((^ 2) . digitToInt) $ show a) == 1 = True
        -- | 
-- /
