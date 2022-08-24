module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n-d) d (count + 1)

myMul :: (Eq a, Num a) => a -> a -> a
myMul 0 y = 0
myMul x y = y + myMul (x-1) y


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "My multiplication function" $ do
        it "3 * 2 is equal 6" $ do
            (myMul 3 2) `shouldBe` 6
        it "1 * 2 is equal 2" $ do
            (myMul 1 2) `shouldBe` 2
        