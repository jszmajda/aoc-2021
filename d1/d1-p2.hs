import System.IO
import Data.List (foldl')

main = do
  contents <- readFile "day1-input.txt"
  let numbers = map read $ lines contents
  let depth = runDepth $ sumWindows 3 numbers
  print depth

sumWindows :: Int -> [Integer] -> [Integer]
sumWindows size = (map sum) . windows size

windows :: Int -> [Integer] -> [[Integer]]
windows size ns = map window [0..(length ns) - size]
  where
    window n = take size $ drop n ns

runDepth :: [Integer] -> Integer
runDepth ns = snd $ foldl' iterDepth (head ns, 0) ns

iterDepth :: (Integer, Integer) -> Integer -> (Integer, Integer)
iterDepth (cur, increases) next = (next, if next > cur then increases + 1 else increases)

