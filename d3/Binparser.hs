import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

readBin :: String -> Integer
readBin str = fst . head $ readInt 2 (`elem` "01") digitToInt str
