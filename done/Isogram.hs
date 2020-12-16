module Isogram where
import Data.Char (toLower)
import qualified Data.Set as S
import qualified Data.List as L
import Control.Applicative(liftA2)


isIsogramCheck :: String -> Bool
isIsogramCheck [] = True
isIsogramCheck (x:xs)
 | x `elem` xs = False
 | otherwise = isIsogram xs
 
isIsogram :: String -> Bool
isIsogram s = isIsogramCheck $ map toLower s


-- @intresting
-- Functor
-- <$> - https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:-60--36--62-
isIsogram2 :: String -> Bool
isIsogram2 = (==) <$> length <*> S.size . S.fromList . map toLower


---
isIsogram3 :: String -> Bool
isIsogram3 s = length l == (length $ S.elems $ S.fromList l)
  where l = map toLower s

  
--- Group by example!
isIsogram4 :: String -> Bool
isIsogram4 = all (==1) . map length . L.group . L.sort . map toLower


-- O(n2) . The nub function removes duplicate elements from a list. In particular, 
-- it keeps only the first occurrence of each element. (The name nub means `essence'.) 
-- It is a special case of nubBy, which allows the programmer to supply their own equality test.
isIsogram5 :: String -> Bool
isIsogram5 = (L.nub >>= (==)) . map toLower

isIsogram6 :: String -> Bool
isIsogram6 = ((==) <*> L.nub) . (map toLower)

-- liftA2
isIsogram :: String -> Bool
isIsogram = liftA2 (==) (length) (S.size . S.fromList) . map toLower