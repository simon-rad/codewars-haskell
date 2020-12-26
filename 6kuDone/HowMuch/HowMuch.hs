module HowMuch where
import Text.Printf ( printf )
import Data.Maybe (mapMaybe)

isIt :: Int -> Maybe [String]
isIt x
  | [snd b, snd c] == [0,0] = Just [printf "M: %d" x, printf "B: %d" . fst $ b, printf "C: %d" . fst $ c]
  | otherwise = Nothing
  where 
    c = quotRem (x-1) 9
    b = quotRem (x-2) 7

howmuch :: Int -> Int -> [[String]]
howmuch m n
 | m <= n = mapMaybe isIt [m..n]
 | otherwise = mapMaybe isIt [n..m]

test = howmuch 10000 9991
