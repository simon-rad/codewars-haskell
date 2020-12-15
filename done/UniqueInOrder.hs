module UniqueInOrder where
import Data.List

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder [a] = [a]
uniqueInOrder [a, b]
 | a /= b = [a,b]
 | otherwise = [a]
uniqueInOrder l@(x:y:xs)
 | x == y = uniqueInOrder $ x : xs
 | otherwise = x : uniqueInOrder (tail l)
 

uniqueInOrder2 :: Eq a => [a] -> [a]
uniqueInOrder2 = map head . group


helper :: Eq a => [a] -> a -> [a]
helper acc x 
  | null acc     = [x]
  | x == last acc = acc
  | otherwise     = acc ++ [x]

uniqueInOrder3 :: Eq a => [a] -> [a]
uniqueInOrder3 = foldl' helper []