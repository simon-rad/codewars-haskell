module FirstVariationOnCaesarCipher where

import Data.Char ( ord, chr, isLower, isUpper )
import Data.List.Split ( chunksOf )

upperStartsAt :: Int
upperStartsAt = ord 'A'

lowerStartsAt :: Int
lowerStartsAt = ord 'a'

lettersCount :: Int
lettersCount = ord 'z' - ord 'a' + 1

moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar 
    charStartsAt 
    modifyNumber 
    characterToMove = chr resultingIndex
    where
        resultingIndex = resultingIndexFrom0 + charStartsAt
        resultingIndexFrom0 = mod (modifyNumber charIndexFrom0) lettersCount
        charIndexFrom0 = charIndex - charStartsAt
        charIndex = ord characterToMove

-- More functional WAY
moveCharFP1 :: Int -> (Int -> Int) -> Char -> Char
moveCharFP1
    charStartsAt 
    modifyNumber  = chr . (+ charStartsAt) . flip mod lettersCount . modifyNumber . flip (-) charStartsAt . ord

-- 100% FP :D
moveCharFP100 :: Int -> (Int -> Int) -> Char -> Char
moveCharFP100 x y = chr . (+ x) . flip mod 26 . y . flip (-) x . ord
    

moveUpperChar :: (Int -> Int) -> Char -> Char
moveUpperChar = moveChar upperStartsAt

moveLowerChar :: (Int -> Int) -> Char -> Char
moveLowerChar = moveChar lowerStartsAt

moveCharBy :: (Int -> Int) -> Char -> Char
moveCharBy i x
 | isUpper x = moveUpperChar i x
 | isLower x = moveLowerChar i x
 | otherwise = x

moveCharacterInPairByIndexModifier :: (Int -> Int -> Int) -> (Int, Char) -> Char
moveCharacterInPairByIndexModifier i x = moveCharBy (i $ fst x) (snd x)

withIndexes :: String -> [(Int, Char)]
withIndexes = zip [0..]

encode :: String -> Int -> String
encode x i = map (moveCharacterInPairByIndexModifier changeIndex) $ withIndexes x
    where changeIndex = \x y -> i + x + y

decode :: String -> Int -> String
decode x i = map (moveCharacterInPairByIndexModifier changeIndex) $ withIndexes x
    where changeIndex = \x y -> y - (i + x)

getDivider :: String -> Int
getDivider x = ceiling ( fromIntegral (length x) / 5 ) :: Int

brakeString :: String -> [String]
brakeString x = chunksOf (getDivider x) x

movingShift :: String -> Int -> [String]
movingShift input moveAt = take 5 $ brakeString (encode input moveAt) ++ [""]

demovingShift :: [String] -> Int -> String
demovingShift input = decode (concat input)
 
-- Examples to test on!
source :: [Char]
source = "I should have known that you would have a perfect answer for me!!!"
expected :: [[Char]]
expected = ["J vltasl rlhr ","zdfog odxr ypw"," atasl rlhr p ","gwkzzyq zntyhv"," lvz wp!!!"]


