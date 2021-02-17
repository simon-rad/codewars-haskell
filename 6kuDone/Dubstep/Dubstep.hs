module Dubstep where

import Data.List.Split (splitOn)

songDecoder :: String -> String
songDecoder = unwords . filter (/= []) . splitOn "WUB"

test = songDecoder "AWUBWUBWUBBWUBWUBWUBC" --  `shouldBe` "A B C"

test2 = songDecoder "WUBAWUBBWUBCWUB" --  `shouldBe` "A B C"

test3 = songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"

-- `shouldBe` "WE ARE THE CHAMPIONS MY FRIEND"
