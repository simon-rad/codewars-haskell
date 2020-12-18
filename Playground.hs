module Playground where

import Data.Char
import Data.List
import Data.List.Split

        
-- demovingShift :: [String] -> Int -> String
-- your code

-- movingShift :: String -> Int -> [String]
-- your code

brakeString :: String -> [[(Int, Char)]]
brakeString x = splitByIndex indexed by
    where 
        by = getDivider x + 1
        indexed = zip [1..] x

splitByIndex :: [(Int, Char)] -> Int -> [[(Int, Char)]]
splitByIndex str i
 | length str <= i = [str]
 | otherwise = cResult : splitByIndex remaining i
 where 
     brakeF = \x -> fst x `mod` i == 0
     broken = break brakeF str
     cResult = fst broken
     remaining = snd broken

getDivider :: String -> Int
getDivider x = ceiling ( fromIntegral (length x) / 5 ) :: Int
        
-- test :: Int -> Int
source = "I should have known that you would have a perfect answer for me!!!"
expected = ["J vltasl rlhr ","zdfog odxr ypw"," atasl rlhr p ","gwkzzyq zntyhv"," lvz wp!!!"]


