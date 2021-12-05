import Data.List
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

readBin :: String -> Integer
readBin str = fst . head $ readInt 2 (`elem` "01") digitToInt str

main = do
  contents <- readFile "input.txt"
  let ls = lines contents

  let oxyRating = bitScrub ls 0 fst
  print oxyRating
  print (readBin oxyRating)
  let scrubRating = bitScrub ls 0 snd
  print scrubRating
  print (readBin scrubRating)

  -- life support rating is oxy * scrub
  print ((readBin oxyRating) * (readBin scrubRating))



bitScrub :: [String] -> Int -> ((Char, Char) -> Char) -> String
bitScrub rows@(x:xs) bitPosition selectorFn
  | bitPosition > length x = error "no result"
  | length xs == 0         = x
  | otherwise              = bitScrub nextRows (bitPosition + 1) selectorFn
  where
    nextRows = filter matchBitCriteria rows
    selector = selectorFn $ mostLeastCommon $ (transpose rows) !! bitPosition
    matchBitCriteria row = (row !! bitPosition) == selector


mostLeastCommon :: [Char] -> (Char, Char)
mostLeastCommon set = if zero > one then ('0', '1') else ('1', '0')
  where
    zero = head counts
    one = head $ tail counts
    counts = map length $ (group . sort) set
