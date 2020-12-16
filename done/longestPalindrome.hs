module Min where
import Data.List

isPalindrome :: String -> Bool
isPalindrome [] = False
isPalindrome [_] = True
isPalindrome [a,b]
 | a == b = True
 | otherwise = False
isPalindrome (x:xs) 
 | x == l = isPalindrome middle
 | otherwise = False
    where
        l = last xs
        middle = init xs


getOneSideChecker :: String -> Int
getOneSideChecker s@(_:xs)
 | isPalindrome s = length s
 | otherwise = getOneSideChecker xs

longestPalindrome :: String -> Int
longestPalindrome [] = 0
longestPalindrome [_] = 1
longestPalindrome full@(x:xs) 
 | length full == 2 && x == l = 2
 | length full == 2 = 1
 | isPalindrome full = length full
 | otherwise = maximum [middleScore, rightSideScore, leftSideScore]
    where
        l = last xs
        middle = init xs
        withouLast = init full
        middleScore = longestPalindrome middle
        rightSideScore = getOneSideChecker xs
        leftSideScore = getOneSideChecker $ reverse withouLast



-- LOL

longestPalindrome2 :: Eq a => [a] -> Int
longestPalindrome2 = maximum . map length . filter (\s -> s == reverse s) . concatMap tails . inits