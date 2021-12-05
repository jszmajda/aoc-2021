import System.IO
import Data.List (foldl')

main = do
  contents <- readFile "day1-input.txt"
  let numberLines = lines contents
  let depth = runDepth (map read numberLines)
  print depth

runDepth :: [Integer] -> Integer
runDepth ns = snd $ foldl' iterDepth (head ns, 0) ns

iterDepth :: (Integer, Integer) -> Integer -> (Integer, Integer)
iterDepth (cur, increases) next = (next, if next > cur then increases + 1 else increases)

