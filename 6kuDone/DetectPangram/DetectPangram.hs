module DetectPangram where
import Data.Char
import Data.List

isIn x = elem x ['a'..'z']

isPangram :: String -> Bool
isPangram str = l == 26
  where l = length . group . sort . filter isIn . map toLower $ str
