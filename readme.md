# Katas from codewars
.
Archive of katas that I have solved

## Katas Log

### The Deaf Rats of Hamelin (6ku)

[Kata link](https://www.codewars.com/kata/598106cb34e205e074000031)

[My Solution](/6kuDone/TheDeafRatsOfHamelin/TheDeafRatsOfHamelin.hs)

Somebody solved with JUST ONE recursion!
Amazing! [Check this variant](/6kuDone/TheDeafRatsOfHamelin/bestOnes/Best2.hs)

### Two Sum (6ku)

[Kata Link](https://www.codewars.com/kata/52c31f8e6605bcc646000082)
[My Solution](/6kuDone/TwoSum/TwoSum.hs)

Learned that you can traverse lists like this

```haskell
allCombinations xs = [(x,y) | x <- xs, y <- xs]
-- allCombinations [1,2,3]
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
```

Also you can use zip [0..] to get indexes!!!

```haskell
tryOutZip list = [(fst x, fst y, snd x + snd y) | x <- zipped, y <- zipped]
    where
        zipped = zip [0..] list
-- tryOutZip [1,2,3]
-- [(0,0,2),(0,1,3),(0,2,4),(1,0,3),(1,1,4),(1,2,5),(2,0,4),(2,1,5),(2,2,6)]
```

Then you can restrict by index, and build only needed combinations

```haskell
tryOutZip list = [(fst x, fst y, snd x + snd y) | x <- zipped, y <- zipped, fst x < fst y]
    where
        zipped = zip [0..] list
-- tryOutZip [1,2,3]
-- [(0,1,3),(0,2,4),(1,2,5)]
```

### Find the odd int (6ku)

[Kata link](https://www.codewars.com/kata/54da5a58ea159efa38000836/)
[My Solution](/6kuDone/FindTheOddInt/FindTheOddInt.hs)

Somebody solved by just using Binary xor

```haskell
import Data.Bits (xor)

findOdd :: [Int] -> Int
findOdd = foldr xor 0
```

for example if we take [5,6,6]

1. 0 `xor` 5 = 5
2. 5 `xor` 6 = 3
3. 3 `xor` 6 = 5

for [2,1,3,2,3]

1. 0 `xor` 2 = 2
2. 2 `xor` 1 = 3
3. 3 `xor` 3 = 0
4. 0 `xor` 2 = 2
5. 2 `xor` 3 = 1

### First Variation on Caesar Cipher 5ku

My First 5ku Kata in Haskell!

[Kata Link](https://www.codewars.com/kata/5508249a98b3234f420000fb)

[My Solution](/5kuDone/FirstVariationOnCaesarCipher/FirstVariationOnCaesarCipher.hs)

Used [Data.List.Split](https://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html) For a first time
`chunksOf` was very helpfull for this task ))

#### To many brackets

Haskell is getting kind of anoying with no brackets, and discoragment of variables...

```haskell
moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar
    charStartsAt
    modifyNumber
    characterToMove = chr (
            (
                (
                    modifyNumber (
                        ord characterToMove
                    ) - charStartsAt
                ) `mod` lettersCount
            ) + charStartsAt
        )
```

Tried to add some variables.
This feels more comfortable, but is this FP way?

```haskell
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
```

#### Fixing the brackets problem, in a haskell way

##### Let's check best answers

```haskell
shift :: Int -> Char -> Char
shift i c | isLower c = chr . (+97) . flip mod 26 . (+) (i - 97) $ ord c
          | isUpper c = chr . (+65) . flip mod 26 . (+) (i - 65) $ ord c
          | otherwise = c
```

This looks like FP!

- `chr . (+97)` instead doing it in layers, we compose!
- `flip mod 26` more composition!
Nice tricks!

##### Let's try to rewrite our function with a new knowledge ))

```haskell
lettersCount :: Int
lettersCount = ord 'z' - ord 'a' + 1

moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar
    charStartsAt
    modifyNumber  = chr . (+ charStartsAt) . flip mod lettersCount . modifyNumber . flip (-) charStartsAt . ord
```

Now it's `FP`

We need one more change to be `100% FP`
make all variable names unreadable, and move some calculations to magic numbers!

```haskell
moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar x y = chr . (+ x) . flip mod 26 . y . flip (-) x . ord
```

Good luck finding out what it does, in a months :D

#### ZipWith magic

Another function in that solution is also interesting
It uses this trick

```haskell
zipWith shift [n..] xs
```

So if we have function

```haskell
shift :: Int -> Int -> Int
shift x y = x * 100 + y
```

And we want to apply this `shift` function to each element of an array of numbers like map but `x` should be index of an element, and y an element value
We can simply do that

```haskell
zipWith shift [1..] $ [5,7,9]
-- [105,207,309]
```

Moreover if we don't know starting index we can use a variable

```haskell
zipWith shift [n, n+1..] $ [5,7,9]
```

In this particular kata, that would help us get rid of passing `Int -> Int -> Int` function to tell how we should change index
And remove `[(Int, Char)]` type from our function

#### Simple composition

When I was finished with kata, I still had one error where I have to return
`["ab", "bc", "sm", "gg", ""]`
I was returning only 4 elements
`["ab", "bc", "sm", "gg"]`
So I fixed this by taking 5 elements and adding one empty at the end

```haskell
take 5 $ encodeAndBrake s i ++ [""]
```

But this could have beed written if FP way using composition, like that!

```haskell
take 5 . (++ [""]) $ encodeAndBrake s i
```

#### quotRem Magic

To get amount of items in a chunk I had to implement this function

```haskell
getDivider :: String -> Int
getDivider x = ceiling ( fromIntegral (length x) / 5 ) :: Int
-- ceiling ( fromIntegral (17) / 5 )
```

It takes list `[Char]`
And checking how many items should be in a piles to get 5 chunks at most!

For example
If length is `17` the five parts will have lengths of `4, 4, 4, 4, 1`
So this function would return `4` that will sufice `chunksOf` method and split list accordingly

But this could have been done more simply by using `quotRem`

```haskell
17 `quotRem` 5
```

quotRem - basically 2 functions called on same arguments and result put in a pair
`(quot x y, rem x y)`

`quot 17 5` == `3`
`rem 17 5` == `2`
So we get `(3,2)`
We increment `sft x + 1` and we are golden

### Statistics for an Athletic Association (6ku)

Was harder than I have expected...

[Kata link](https://www.codewars.com/kata/55b3425df71c1201a800009c)
[My solution](/6kuDone/StatisticsForAnAthleticAssociation/StatisticsForAnAthleticAssociation.hs)

#### Used Data Structure for a first time

```haskell
data DateHMS = DateHMS Int Int Int deriving (Show)
```

It's quite eazy to work with those, to extract values you can simmply do that in arguments by diconstruction

```haskell
fromDateHMStoSecnds :: DateHMS -> Int
fromDateHMStoSecnds (DateHMS h m s) = convertToMiliseconds h m s
```

Creating an instance `DateHMS 0 0 0`

```haskell
convertStringToHMS i
 | length spl == 3 = DateHMS (readInt 0) (readInt 1) (readInt 2)
 | otherwise = DateHMS 0 0 0
```

### Zip With

I wrote this f to convert from HH:mm:ss to milliseconds, it works but kind of ugly

```haskell
convertToMiliseconds :: Int -> Int -> Int -> Int
convertToMiliseconds h m s = s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000
```

I found this function in best solutions section

```haskell
timeToSec :: String -> Int
timeToSec = sum . zipWith (*) [3600,60,1] . map read . splitOn "|"
```

It kind of does to many things as for me, but `zipWith` trick cood be used in my function

```haskell
convertToMiliseconds :: Int -> Int -> Int -> Int
convertToMiliseconds h m s = sum . zipWith (*) [1000, 1000*60, 1000*60*60] $ [s, m, h]
```

And as ussual it's not 100% FP if we don't have `magic numbers` and `unreadable function/variable names`

```haskell
convToMs :: Int -> Int -> Int -> Int
convToMs x y z = sum . zipWith (*) [1000, 60000, 3600000] $ [z, y, x]
```

Now we golden :D

#### printf

To construct part of report string I have used this helper

```haskell
fromDateHMSToReport :: String -> DateHMS -> String
fromDateHMSToReport prefix (DateHMS h m s) = prefix ++ withLeadingZero h ++ "|" ++ withLeadingZero m ++ "|" ++ withLeadingZero s
```

And used it to make final string

```haskell
getRangeStats :: [Char] -> String
getRangeStats = getStats "Range: " range

getAverageStats :: [Char] -> String
getAverageStats = getStats " Average: " average

getMedianStats :: [Char] -> String
getMedianStats = getStats " Median: " median

-- And then doing this
concat [getRangeStats x, getAverageStats x, getMedianStats x]
```

Let's make it look better with help of `printf`

We start with a helper, we remove prefix, and use printf

```haskell
fromDateHMSToReport :: DateHMS -> String
fromDateHMSToReport (DateHMS h m s) =
    printf "%s|%s|%s" (withLeadingZero h) (withLeadingZero m) (withLeadingZero s)
```

And as ussual make variable/functionNames unreadable to reach 100% FP

```haskell
frDtToRep :: DateHMS -> String
frDtToRep (DateHMS h m s) =
    printf "%s|%s|%s" (wthLd0 h) (wthLd0 m) (wthLd0 s)
```

And instead of concat we now have

```haskell
getResult :: String -> String
getResult x = printf "Range: %s Average: %s Median: %s" r a m
  where
    r = getStats range x
    a = getStats average x
    m = getStats median x
```

#### Using quotRem VS divMod

To get HH:mm:ss back I wrote this function

```haskell
fromSecondsToDateHMS :: Int -> DateHMS
fromSecondsToDateHMS seconds = DateHMS h m s
    where
        (h,mLeft) = quotRem seconds (60*60 * 1000)
        (m,ms) = quotRem mLeft (60 * 1000)
        (s, _) = quotRem ms 1000
```

In one of the solutions I saw somebody using `divMod`
So what the difference?

`quotRem` - simultaneous `quot` and `rem`

- `quotRem 157 50 = (3,7)`
- `quotRem (-157) 50 = (-3, -7)`
- `quotRem (-157) (-50) = (3,-7)`

`divMod` - simultaneous `div` and `mod`

- `divMod 157 50 = (3,7)`
- `divMod (-157) 50 = (-4,43)`
- `divMod (-157) (-50) = (3, -7)`

Where
`quot` - integer division truncated toward zero

- `quot 86 10 = 8`

`div` - integer division truncated toward negative infinity

- `div 86 10 = 8`

The difference comes with negative numbers

- `div (-86) 10 = -9` - truncated toward negative infinity
- `quot (-86) 10 = -8` - truncated toward zero

`rem` - integer remainder

- `rem 86 10 = 6`
- `mod (-1700) 1000 = -700`

`mod` - integer modulus

- `mod 86 10 = 6`
- `mod (-1700) 1000 = 300`

So in this particular case, as we don't work with negative numbers, it doesn't matter

### Detect Pangram (6ku)

Eazy kata should be 7 or 8 ku

[Kata link](https://www.codewars.com/kata/545cedaa9943f7fe7b000048)
[My Solution](/6kuDone/DetectPangram/DetectPangram.hs)

### Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!! (6ku)

[Kata link](https://www.codewars.com/kata/5626b561280a42ecc50000d1)
[My Solution](/6kuDone/TakeNumberAndSumItsDigitsRaisedToTheConsecutivePowers/TakeNumberAndSumItsDigitsRaisedToTheConsecutivePowers.hs)

Used zipWith that I have learned before ))

```haskell
checkNumb :: Int -> Bool
checkNumb x = t == x
  where t = sum . (flip . zipWith) (^) [1..] . map digitToInt $ show x
```

#### Using == as a function

You can use `(== n)` as a function in composition, instead of using variable

```haskell
f n = (== n) . sum . zipWith (flip (^)) [1..] . map digitToInt . show $ n
```

### Reverse or rotate? (6ku)

[Kata Link](https://www.codewars.com/kata/56b5afb4ed1f6d5fb0000991)
[My Solution](/6kuDone/ReverseOrRotate/ReverseOrRotate.hs)

#### Filtering  by length

Instead of

`filter (\i -> length i == sz)`

We can use composition

`filter ((== n) . length)`

### How Much (6ku)

It seems like understanding the description is more chalanging than actually solving the kata

[Kata Link](https://www.codewars.com/kata/55b4d87a3766d9873a0000d4)
[My Solution](/6kuDone/HowMuch/HowMuch.hs)

#### Mapping Maybe tipe

Quite convinietly instead of doing

```haskell
map fromJust . filter isJust . map isIt $ [n..m]
```

you can use `mapMaybe`
That will filter out Notings and map your List

```haskell
mapMaybe isIt [n..m]
```

### TitleCase (6ku)

An eazy one, straight forward solution

[Kata Link](https://www.codewars.com/kata/5202ef17a402dd033c000009/s)
[My Solution](/6kuDone/TitleCase/TitleCase.hs)
