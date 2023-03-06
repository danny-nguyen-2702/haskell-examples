{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Move = GoLeft | GoForward | GoRight
data Forest a = FoundExit | Passage a (Forest a) (Forest a) (Forest a) deriving (Show)


move :: (Num a) => (a, Forest a) -> Move -> (a, Forest a)
move (x, FoundExit) _                           = (x, FoundExit)
move (x, Passage y leftForest _ _) GoLeft       = (x + y, leftForest)
move (x, Passage y _ forwardForest _) GoForward = (x + y, forwardForest)
move (x, Passage y _ _ rightForest) GoRight     = (x + y, rightForest)

testForest :: (Num a) => Forest a
testForest = Passage 1 
    (
        Passage 2 
        (Passage 1 FoundExit FoundExit FoundExit) 
        FoundExit 
        (Passage 3 FoundExit FoundExit FoundExit)
    ) 
    (
        Passage 3 
        (Passage 2 FoundExit FoundExit FoundExit) 
        (Passage 1 FoundExit FoundExit FoundExit) 
        (Passage 5 FoundExit FoundExit FoundExit)) 
    (
        Passage 4 
        (Passage 7 FoundExit FoundExit FoundExit) 
        (Passage 3 FoundExit FoundExit FoundExit) 
        (Passage 6 FoundExit FoundExit FoundExit)
    )


solveForest :: (Num a, Ord a, Show a) => Forest a -> [Move] -> String
solveForest forest moves = showCurrentChoice $ foldl move (0, forest) moves

showCurrentChoice :: (Num a, Ord a, Show a) => (a, Forest a) -> String
showCurrentChoice (x, _) | x > 10 = "You ran out of stamina and died -.-!"
showCurrentChoice (x, FoundExit) = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice (x, _) = "You have " ++ show (10 - x) ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
