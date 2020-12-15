module Codewars.G964.FindEven where
-- import Control.Applicative ((<$>), (<*>))
import Data.List ( elemIndex, findIndex )
import Data.Maybe ( fromMaybe, isJust, fromJust )

addSum :: [Int] -> Int -> [Int]
addSum [] _ = []
addSum (x:xs) sum = sum : addSum xs  (sum + x)


findEven :: [Int] -> Maybe Int
findEven list = findIndex (\(x,y) -> x == y) $ zip (reverse $ addSum (reverse list) 0) (addSum list 0)

findEvenIndex :: [Int] -> Int
findEvenIndex list
 | isJust result = fromJust result
 | otherwise = -1
    where result = findEven list


-- testFind [1,2,3,4,3,2,1] 3
-- testFind [1,100,50,-51,1,1] 1
-- testFind [1,2,3,4,5,6] (-1)
-- testFind [20,10,30,10,10,15,35] 3
-- testFind [20,10,-80,10,10,15,35] 0
-- testFind [10,-80,10,10,15,35,20] 6
-- testFind [0,0,0,0,0] 0
-- testFind [-1,-2,-3,-4,-3,-2,-1] 3

-- OMG
-- TODO learn about this!

findEvenIndex2 :: [Int] -> Int
findEvenIndex2 = fromMaybe (-1) . elemIndex True .
  (zipWith (==) <$> scanl1 (+) <*> scanr1 (+))

findEvenIndex3 :: [Int] -> Int
findEvenIndex3 x = 
    fromMaybe (-1) (  -- If not found return -1 else value of Just
        elemIndex True ( -- Basicaly findIndex where we have true
            zipWith (==) ( -- This will combine 2 arrays using ===
                           -- [False, False, True, False, False]
                scanl1 (+) x -- Go from left side, and summ each element with a previous one
            ) (
                scanr1 (+) x -- same as above but from right side
            )
        ) 
    )

findEvenIndex4 :: [Int] -> Int
findEvenIndex4 x = convertedFromMaybe
    where
        -- If not found return -1 else value of Just
        convertedFromMaybe = fromMaybe (-1) filtered
        -- Basicaly findIndex where we have true
        filtered = elemIndex True zipped
        -- This will combine 2 arrays using ===
        -- [False, False, True, False, False]
        zipped = zipWith (==) fromLeftSum fromRightSum
        -- Same as below but from left side
        fromLeftSum = scanl1 (+) x
        -- Go From right side adding prevValue with current value
        fromRightSum = scanr1 (+) x

-- zipPair :: Num a => a -> a -> a
-- zipPair a b = a * 10000 + b * 100

-- findEvenIndex2 x = zipWith zipPair (scanl1 (+) x) (scanr1 (+) x)