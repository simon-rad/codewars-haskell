module StatisticsForAnAthleticAssociation where
import Data.List (sort)
import Data.List.Split ( splitOn )

average :: [Int] -> Int
average x = round $ s / len
    where
        s = fromInteger . toInteger $ sum x
        len = (fromInteger $ toInteger $ length x) :: Float

range :: [Int] -> Int
range x = maximum x - minimum x

median :: [Int] -> Int
median x
 | odd l = xs !! medianIndex
 | even l = average [(!!) xs . flip (-) 1 $ medianIndex, (!!) xs medianIndex]
  where
      xs = sort x
      l = length x
      medianIndex = fromInteger . flip div 2 . toInteger $ l

data DateHMS = DateHMS Int Int Int deriving (Show) 

convertToMiliseconds :: Int -> Int -> Int -> Int
convertToMiliseconds h m s = s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000

fromDateHMStoSecnds :: DateHMS -> Int
fromDateHMStoSecnds (DateHMS h m s) = convertToMiliseconds h m s

fromSecondsToDateHMS :: Int -> DateHMS
fromSecondsToDateHMS seconds = DateHMS h m s
    where 
        (h,mLeft) = quotRem seconds (60*60 * 1000)
        (m,ms) = quotRem mLeft (60 * 1000)
        (s, _) = quotRem ms 1000


convertStringToHMS :: String -> DateHMS
convertStringToHMS i
 | length spl == 3 = DateHMS (readInt 0) (readInt 1) (readInt 2)
 | otherwise = DateHMS 0 0 0
    where 
        spl = splitOn "|" i
        readInt indx = read (spl !! indx) :: Int

withLeadingZero :: Int -> String 
withLeadingZero x
 | length (show x) == 1 = '0' : show x
 | otherwise = show x

fromDateHMSToReport :: String -> DateHMS -> String
fromDateHMSToReport prefix (DateHMS h m s) = prefix ++ withLeadingZero h ++ "|" ++ withLeadingZero m ++ "|" ++ withLeadingZero s

getStats :: String -> ([Int] -> Int) -> [Char] -> String
getStats prefix statsF = fromDateHMSToReport prefix . fromSecondsToDateHMS . statsF . map (fromDateHMStoSecnds . convertStringToHMS) . splitOn ", "

getRangeStats :: [Char] -> String
getRangeStats = getStats "Range: " range

getAverageStats :: [Char] -> String
getAverageStats = getStats " Average: " average

getMedianStats :: [Char] -> String
getMedianStats = getStats " Median: " median

stat :: [Char] -> [Char]
stat x
 | x == "" = ""
 | otherwise = concat [getRangeStats x, getAverageStats x, getMedianStats x]

-- Test samples

raw :: [Char]
raw = "01|15|59, 1|47|16, 01|17|20, 1|32|34, 2|17|17"
result :: [Char]
result = "Range: 01|01|18 Average: 01|38|05 Median: 01|32|34"

raw2 :: [Char]
raw2 = "02|15|59, 2|47|16, 02|17|20, 2|32|34, 2|17|17, 2|22|00, 2|31|41"

result2 :: [Char]
result2 = "Range: 00|31|17 Average: 02|26|18 Median: 02|22|00"

raw3 = "12|17|48, 11|15|17, 2|17|17, 1|22|00, 1|15|17, 00|22|34"
result3 = "Range: 11|55|14 Average: 04|48|22 Median: 01|49|38"
-- but got "Range: 11|55|14 Average: 04|48|22 Median: 06|46|17