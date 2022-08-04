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