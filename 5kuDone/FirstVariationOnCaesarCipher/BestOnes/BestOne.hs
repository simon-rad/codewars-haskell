module FirstVariationOnCaesarCipherBestOne where

import Data.Char
import Data.List
import Data.List.Split

        
demovingShift :: [String] -> Int -> String
demovingShift xs n = zipWith shift [-n,-n-1..] $ concat xs

movingShift :: String -> Int -> [String]
movingShift xs n = case length xs `quotRem` 5 of
    (x,0) -> chunksOf x $ zipWith shift [n..] xs
    (x,_) -> take 5 . (++ [""]) . chunksOf (x + 1) $ zipWith shift [n..] xs

shift :: Int -> Char -> Char
shift i c | isLower c = chr . (+97) . flip mod 26 . (+) (i - 97) $ ord c
          | isUpper c = chr . (+65) . flip mod 26 . (+) (i - 65) $ ord c
          | otherwise = c
