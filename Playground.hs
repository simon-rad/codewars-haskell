module Playground where

import Data.List.Split (splitOn)

songDecoder :: String -> String
songDecoder = unwords . filter (/= []) . splitOn "WUB"

test = songDecoder "AWUBWUBWUBBWUBWUBWUBC"
