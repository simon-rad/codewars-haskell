module TakeNumberAndSumItsDigitsRaisedToTheConsecutivePowers where
import Data.Char ( digitToInt )

checkNumb :: Int -> Bool
checkNumb x = t == x
  where t = sum . (flip . zipWith) (^) [1..] . map digitToInt $ show x

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b
  | a >= b = []
  | a < 0 || b < 0 = []
  | otherwise = filter checkNumb [a..b]
