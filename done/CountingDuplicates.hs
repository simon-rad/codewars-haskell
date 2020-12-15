import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map

myDuplicateCount :: String -> Int
myDuplicateCount [] = 0
myDuplicateCount (x:xs)
 | x `elem` xs = 1 + ( myDuplicateCount ( filter (/= x) xs ) )
 | otherwise = myDuplicateCount xs

duplicateCount :: String -> Int
duplicateCount = myDuplicateCount . map toLower


duplicateCount1 :: String -> Int
duplicateCount1 = length . filter ((> 1) . length) . L.group . L.sort . map toLower


duplicateCount2 :: String -> Int
duplicateCount2 = 
  length . Map.filter (>1) . foldr (
      \c m -> Map.insertWith (+) c 1 m
  ) Map.empty . map toLower