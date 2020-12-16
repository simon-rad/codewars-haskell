module CountDeafRats where

-- WOW

countDeafRats :: String -> Int
countDeafRats [] = 0
countDeafRats (x:xs)
  | x == 'P' = (countDeafRats . reverse) xs
  | x == ' ' = countDeafRats xs
  | x == '~' = countDeafRats (tail xs)
  | x == 'O' = 1 + countDeafRats (tail xs)