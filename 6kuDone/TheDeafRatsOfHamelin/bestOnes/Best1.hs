module CountDeafRats where
-- TODO I don't have this modele!
import Data.List.Split

-- TODO check how `splitOn` & `chunksOf` works! 
countDeafRats :: String -> Int
countDeafRats xs = frontDeaf + backDeaf where
  front : back : _ = map (chunksOf 2) $ splitOn "P" $ filter (`elem` "~OP") xs
  frontDeaf = length $ filter (== "O~") front
  backDeaf = length $ filter (== "~O") back
