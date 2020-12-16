module TwoSum (twoSum) where
import Data.List ( find,findIndex,  elemIndex )
import Data.Maybe ( fromMaybe, isJust, fromJust )

findSum :: Int -> [Int] -> Maybe (Int, Int)
findSum _ [] = Nothing
findSum _ [a] = Nothing
findSum sumToFind (x:xs)
 | isJust indexFound = Just (x, fromJust indexFound)
 | otherwise = findSum sumToFind xs
   where 
       indexFound = findIndex (\i -> i + x == sumToFind) xs

twoSum :: [Int] -> Int -> (Int,Int)
twoSum list x = (leftIndex, rightIndex)
    where 
        rightIndex = leftIndex + valueIndex + 1
        leftIndex = fromMaybe (-1) (elemIndex leftValue list)
        (leftValue, valueIndex) = fromMaybe (-1, -1) $ findSum x list

tryOutZip list = [(fst x, fst y, snd x + snd y) | x <- zipped, y <- zipped, fst x < fst y]
    where
        zipped = zip [0..] list


allCombinations xs = [(x,y) | x <- xs, y <- xs]
