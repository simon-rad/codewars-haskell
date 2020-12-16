removeVowels :: Char -> String
removeVowels char 
 | char == 'a' = ""
 | char == 'e' = ""
 | char == 'i' = ""
 | char == 'o' = ""
 | char == 'u' = ""
 | char `elem` ['A', 'E', 'I', 'O', 'U'] = ""
 | otherwise = [char]

disemvowel :: String -> String
disemvowel str = concat $ map (removeVowels) str

