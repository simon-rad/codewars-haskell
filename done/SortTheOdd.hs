module SortArray where
import Data.List (sort)
import Prelude

isOdd :: Int -> Bool
isOdd x = (x `rem` 2) /= 0

filterEvensAndSort :: [Int] -> [Int]
filterEvensAndSort = sort . filter isOdd

mergeOnlyOds :: [Int] -> [Int] -> [Int]
mergeOnlyOds [] _ = []
mergeOnlyOds full [] = full
mergeOnlyOds (fs:fxs) evens@(es:exs)
 | isOdd fs = es : mergeOnlyOds fxs exs
 | otherwise = fs : mergeOnlyOds fxs evens

sortArray :: [Int] -> [Int]
sortArray [] = []
sortArray list = mergeOnlyOds list (filterEvensAndSort list)



-- Todo check this out
sortArray2 :: [Int] -> [Int]
sortArray2 = replaceOdd <$> id <*> sort . filter odd
  where replaceOdd xs [] = xs
        replaceOdd (x:xs) oos@(o:os)
          | even x    = x : replaceOdd xs oos
          | otherwise = o : replaceOdd xs os
