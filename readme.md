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

Let's check best answers

```haskell
lettersCount :: Int
lettersCount = ord 'z' - ord 'a' + 1

shift :: Int -> Char -> Char
shift i c | isLower c = chr . (+97) . flip mod 26 . (+) (i - 97) $ ord c
          | isUpper c = chr . (+65) . flip mod 26 . (+) (i - 65) $ ord c
          | otherwise = c
```

Wow, this feels like FP now!
ok my `chr . (+97)` my bad, could have done the same in my function

Nice trick `flip mod 26`, could have done the same in my kata solution.

Let's try to rewrite our function with a new knowledge ))

```haskell
lettersCount :: Int
lettersCount = ord 'z' - ord 'a' + 1

moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar 
    charStartsAt 
    modifyNumber  = chr . (+ charStartsAt) . flip mod lettersCount . modifyNumber . flip (-) charStartsAt . ord
```

Now it's FP

We need one more change to be 100 FP
make all variable names unreadable, and move some calculations to magic numbers!

```haskell
moveChar :: Int -> (Int -> Int) -> Char -> Char
moveChar x y = chr . (+ x) . flip mod 26 . y . flip (-) x . ord
```

Good luck finding out what it does, in a months :D
