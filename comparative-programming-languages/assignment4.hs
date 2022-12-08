-- In this assignment, your task is to create a postfix calculator, that implements all of the operations described below. In a postfix calculator, expressions are written using postfix notation, i.e. the operator comes after the operands.

-- For example, the postfix expression 2 3 + evaluates to 5, i.e. the same thing as 2 + 3. The infix expression 1/2 + 1/3 would be written 1 2 / 1 3 / + in postfix.

-- Postfix expressions don't use brackets, and don't use any operator precedence rules. For example, the infix expression 3 * (5 - 2) would be written 5 2 - 3 * in postfix.

-- For this assignment, we define tokens as follows:
-- Numbers. These are Haskell Double values.
-- Operators. An operator is a function that does something to the stack. All the operators are defined below. The name of an operator is a string consisting of 1, or more, characters, that doesn’t start with a number, and contains no spaces.

supportedOperators :: [String]
supportedOperators = ["+", "-", "*", "/"]

data Token = Num Double | Operator String deriving (Show)

type Stack = [Double]

putNumToStack :: Token -> Stack -> Stack
putNumToStack (Operator _) stack = stack
putNumToStack (Num x) stack = x : stack

applyOperatorToStack :: Token -> Stack -> Stack
applyOperatorToStack (Num _) stack = stack
applyOperatorToStack (Operator x) stack
    | x == "+" = drop 2 stack ++ [fstOperand + sndOperand]
    | x == "-" = drop 2 stack ++ [fstOperand - sndOperand]
    | x == "*" = drop 2 stack ++ [fstOperand * sndOperand]
    | x == "/" = drop 2 stack ++ [fstOperand / sndOperand]
    | otherwise = error "Dont support this operator"
    where fstOperand = stack !! 1
          sndOperand = stack !! 0

calcStack :: [Token] -> Stack -> Stack
calcStack [] stack = stack
calcStack ((Operator x):ts) stack = calcStack ts (applyOperatorToStack (Operator x) stack)
calcStack ((Num x):ts) stack = calcStack ts (putNumToStack (Num x) stack)


calc :: String -> Double
calc "" = 0
calc str = head $ calcStack expression []
    where expression = foldr (\x acc -> if x `elem` supportedOperators then Operator x : acc else Num (read x :: Double) : acc) [] (words str)

