-- Count the number of occurrences of each character and return it as a list of tuples in order of appearance. For empty output return an empty list.
-- Example:
-- orderedCount "abracadabra" == [('a', 5), ('b', 2), ('r', 2), ('c', 1), ('d', 1)]

import qualified Data.Map as Map

orderedCount :: String -> [(Char, Int)]
orderedCount str = sortByAppearance . Map.toList $ foldr (\c acc -> Map.insertWith (+) c 1 acc) Map.empty str

sortByAppearance :: Ord b => [(a, b)] -> [(a, b)]
sortByAppearance [] = []
sortByAppearance ((k, v):xs) = biggers ++ [(k, v)] ++ smallers
    where biggers = sortByAppearance $ filter (\(k', v') -> v' >= v) xs
          smallers = sortByAppearance $ filter (\(k', v') -> v' < v) xs
