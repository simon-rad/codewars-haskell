module Codewars.BuildTower where


cP :: Int -> String
cP n = map (\_->' ') [1..n]

getFloor :: Int -> String
getFloor n
 | n <= 1 = "*"
 | otherwise = '*' : getFloor(n - 1)
 
addPadding :: Int -> String -> String
addPadding n s = cP n ++ s ++ cP n


buildTower :: Int -> [String]
buildTower n = map mapper startList
  where
    list = take n [1,3..]
    nMax = last list
    padding max current =  (max - current) `div` 2
    mapper x = addPadding (padding nMax (length x)) x
    startList = map getFloor list



buildTower2 :: Int -> [String]
buildTower2 n = [(sp x++ st x++ sp x) | x <- [1..n]]
    where sp x = replicate (n-x) ' '
          st x = replicate (2*x-1) '*'