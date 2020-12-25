module ReverseOrRotate where
import Data.Char ( digitToInt )
import Data.List.Split

moveByOne :: String -> String 
moveByOne (x:xs) = xs ++ [x]

isSumOfCubesDivisibleBy2 :: String -> Bool
isSumOfCubesDivisibleBy2 = even . sum . map ((^3). digitToInt)

reverseOrRotate :: String -> String
reverseOrRotate x
  | isSumOfCubesDivisibleBy2 x = reverse x
  | otherwise = moveByOne x

revRot :: [Char] -> Int -> [Char]
revRot strng sz
 | length strng < sz = ""
 | sz <= 0 = ""
 | otherwise = concatMap reverseOrRotate . filter (\i -> length i == sz). chunksOf sz $ strng
