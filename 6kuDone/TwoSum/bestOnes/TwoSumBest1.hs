module TwoSum (twoSum) where

twoSum :: [Int] -> Int -> (Int,Int)
twoSum xxs n = head [(fst x,fst y) | x <- xs, y <- xs, snd x + snd y == n, fst x < fst y]
  where xs = zip [0..] xxs