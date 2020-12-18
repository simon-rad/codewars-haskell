module Codewars.Kata.FindOdd where
import Data.List ( find, sort, group )
import Data.Maybe ( fromMaybe )

-- | Given a list, find the [Int] that appears an 
--   odd number of times. The tests will always
--   provide such a number, and the list will
--   always contain at least one element.
findOdd :: [Int] -> Int
findOdd xs = head $ fromMaybe [-1] $ find (\x -> length x `mod` 2 == 1 ) $ group $ sort xs