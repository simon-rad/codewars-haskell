module Playground where
import Data.Ratio ( (%), denominator, numerator )
import Data.Either ()

toWeirdFormatThatKataAsks :: (Integer, Integer) -> Either Integer (Integer, Integer)
toWeirdFormatThatKataAsks x
  | snd x == 1 = Left $ fst x
  | otherwise = Right x


game :: Integer -> Either Integer (Integer, Integer)
game x = toWeirdFormatThatKataAsks (numerator f, denominator f)
  where f = sum . concatMap (\i -> [i % ix | ix <-[(i+1)..(i+x)]]) $ [1..x]
