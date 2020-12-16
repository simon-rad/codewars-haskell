module Codewars.Kata.NthSeries where
import Data.List ( maximumBy )

charScore :: Enum a => a -> Int
charScore x = fromEnum x - 96

wordScore :: String -> Int
wordScore = sum . map charScore

mySort :: String -> String -> Ordering
mySort left right
 | score == 0 = EQ
 | score > 0 = LT
 | score < 0 = GT
    where
        score = wordScore right - wordScore left

high :: String -> String
high x
 | null x = x
 | otherwise = maximumBy mySort $ words x
