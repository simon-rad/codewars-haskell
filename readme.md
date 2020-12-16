# Katas from codewars

Archive of katas that I have solved

## 6ku done

### The Deaf Rats of Hamelin

[Kata link](https://www.codewars.com/kata/598106cb34e205e074000031)

[MySolution](/6kuDone/TheDeafRatsOfHamelin/TheDeafRatsOfHamelin.hs)

Somebody solved with JUST ONE recursion!
Amazing! [Check this variant](/6kuDone/TheDeafRatsOfHamelin/bestOnes/Best2.hs)

### Two Sum

[Link](https://www.codewars.com/kata/52c31f8e6605bcc646000082)
[MySolution](/6kuDone/TwoSum/TwoSum.hs)

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
