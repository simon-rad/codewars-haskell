module CountDeafRats where
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )

score :: Num a => [Char] -> a
score [] = 0
score [_] = 0
score (x:xs)
 | x == '~' = 0 + score tl
 | x == 'O' = 1 + score tl
 | otherwise = 0
    where tl = tail xs
    
countDeafRats :: String -> Int
countDeafRats x = score a + score (reverse b)
    where
      (a,b) = splitAt idx xf
      xf = filter (/= ' ') x
      idx = fromMaybe (-1) (elemIndex 'P' xf)


test :: Int
test = countDeafRats "P O~ O~ ~O O~" -- 1
test33 :: Int
test33 = countDeafRats "~O~OO~~O~O O~ P O~ O~ ~O O~~O~O~O~O"
test2 :: Int
test2 = countDeafRats "~O~O~O~OP~O~OO~" -- 2