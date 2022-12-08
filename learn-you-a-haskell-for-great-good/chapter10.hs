-------------------------------------- Functionally Solving Problems --------------------------------------

-------------- Reverse Polish notation calculator

-- expression written in infix manner: 10 - (4 + 3) * 2
-- same expression written in RPN: 10 4 3 + 2 * -

calculateRPN :: String -> Float
calculateRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "+" = (y + x) : ys
            foldingFunction (x:y:ys) "-" = (y - x) : ys
            foldingFunction (x:y:ys) "*" = (y * x) : ys
            foldingFunction (x:y:ys) "/" = (y / x) : ys
            foldingFunction (x:y:ys) "^" = (y ** x) : ys
            foldingFunction (x:ys) "ln" = log x : ys
            foldingFunction xs numberStr = read numberStr : xs

-------------- Heathrow to London
aRoad :: (Integral a) => [a]
aRoad = [0, 50, 5, 40, 10]

bRoad :: (Integral a) => [a]
bRoad = [0, 10, 90, 2, 8]

crossRoads :: (Integral a) => [a]
crossRoads = [0, 30, 20, 25 ,0]

data Road = A | B deriving (Show, Eq)
type Index = Int
type Point = (Road, Index) 
type Path = [Point]

findPath :: (Integral a, Read a, Show a) => a -> Path -> (a, Path)
findPath _ [] = findPath 0 [(A, 0)]
findPath currentTotalPrice currentPath@((road,index):xs)
    | index == length aRoad - 1 = (currentTotalPrice, currentPath) -- if found the destination, return the path
    | road == A = 
        let 
            nextForwardPrice = currentTotalPrice + aRoad !! (index+1) 
            nextForwardPoint = (A, index+1) -- we can go forward by increase the index by 1, if our current point is (A,0), our next forward point is (A,1)
            nextCrossPrice = currentTotalPrice + crossRoads !! index
            nextCrossPoint = (B, index) -- or we can go cross to the oposite road, if our current point is (A,0), our cross point is (B,0)
            (forwardPrice, forwardPath) = if nextForwardPoint `elem` currentPath then (100000000, nextForwardPoint:currentPath) else findPath nextForwardPrice (nextForwardPoint:currentPath)
            (crossPrice, crossPath) = if nextCrossPoint `elem` currentPath then (100000000, nextCrossPoint:currentPath) else findPath nextCrossPrice (nextCrossPoint:currentPath)
        in if forwardPrice < crossPrice then (forwardPrice, forwardPath) else (crossPrice, crossPath)
    | road == B = 
        let 
            nextForwardPrice = currentTotalPrice + bRoad !! (index+1)
            nextForwardPoint = (B, index+1) -- we can go forward by increase the index by 1, if our current point is (B,0), our next forward point is (B,1)
            nextCrossPrice = currentTotalPrice + crossRoads !! index
            nextCrossPoint = (A, index) -- or we can go cross to the oposite road, if our current point is (B,0), our cross point is (A,0)
            (forwardPrice, forwardPath) = if nextForwardPoint `elem` currentPath then (100000000, nextForwardPoint:currentPath) else findPath nextForwardPrice (nextForwardPoint:currentPath)
            (crossPrice, crossPath) = if nextCrossPoint `elem` currentPath then (100000000, nextCrossPoint:currentPath) else findPath nextCrossPrice (nextCrossPoint:currentPath)
        in if forwardPrice < crossPrice then (forwardPrice, forwardPath) else (crossPrice, crossPath)
    | otherwise = (currentTotalPrice, currentPath) -- doesn't matter, just use otherwise to get rid of "incomplete-patterns" warning

