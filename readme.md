# Katas from codewars

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
