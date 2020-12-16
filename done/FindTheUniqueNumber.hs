module CodeWars.UniqueNumber where
import qualified Data.List as L
import qualified Data.Maybe as M

getUnique :: [Int] -> Int
getUnique (x:xs)
 | x == head xs = M.fromJust $ L.find (/=x) xs
 | x /= head xs && head xs == head ( tail xs ) = x
 | otherwise = head xs


getUnique2 :: [Int] -> Int
getUnique2 xs@(x:y:z:_) = M.fromJust $ L.find (/= if x == y then x else z) xs 