import Data.List
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

readBin :: String -> Integer
readBin str = fst . head $ readInt 2 (`elem` "01") digitToInt str

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let bits = transpose ls
  let mls = map mostLeastCommon bits
  let gamma = map fst mls
  print gamma
  print (readBin gamma)
  let epsilon = map snd mls
  print epsilon
  print (readBin epsilon)

mostLeastCommon :: [Char] -> (Char, Char)
mostLeastCommon set = if zero > one then ('0', '1') else ('1', '0')
  where
    zero = head counts
    one = head $ tail counts
    counts = map length $ (group . sort) set
