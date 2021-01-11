module TitleCase where
import Data.Char ( toLower, toUpper )

capitalizeIfNotException :: String -> String -> String
capitalizeIfNotException ex string
 | elem (map toLower string) . words . map toLower $ ex = lower
 | otherwise = capitalizeOnlyFirstLetter lower
 where lower = map toLower string

capitalizeOnlyFirstLetter :: String -> String
capitalizeOnlyFirstLetter [] = []
capitalizeOnlyFirstLetter (a:z) = toUpper a : z

titleCase :: String -> String -> String
titleCase minor = capitalizeOnlyFirstLetter . unwords . map (capitalizeIfNotException minor) . words
