module Codewars.Kata.BreakCamelCase where
import Data.List ( groupBy )
import Data.Char ( isLower, isUpper )

addWhitespace :: Char -> String
addWhitespace x
 | isUpper x = ' ' : [x]
 | otherwise = [x]

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace [] = []
trimLeadingWhitespace (' ':xs) = xs
trimLeadingWhitespace x = x

solution :: String -> String
solution = trimLeadingWhitespace . concatMap addWhitespace

--- groupBy example
solution2 :: String -> String
solution2 = unwords . groupBy (\x y -> isLower y)