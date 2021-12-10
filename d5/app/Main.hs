{-# LANGUAGE BangPatterns #-}

module Main where

import Matrix
import LineSegment
import qualified Data.Text as T
import Data.List (foldl')
import Debug.Trace
----import Data.Matrix (Matrix, getElem, setElem, zero, prettyMatrix, nrows, ncols, toList)

main = do
  contents <- lines <$> readFile "input.txt"
  print "read file"
  let segments = map parseLineSegment contents
  print "parsed segments"
  let mp = scribedMap segments
  --putStr $ pretty mp
  print "count with >= 2"
  print $ count (\e -> e >= 2) (toList mp)

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

type Map = Matrix


parseLineSegment :: String -> LineSegment
parseLineSegment input = constructLS $ arrHalves
  where
    firstSplit = T.splitOn (T.pack " -> ") (T.pack input)
    splitCoord seg = T.splitOn (T.pack ",") seg
    xy :: T.Text -> [Int]
    xy seg = map ((read :: String -> Int) . T.unpack) (splitCoord seg)
    arrHalves :: [[Int]]
    arrHalves = map xy firstSplit
    constructLS lar = LineSegment { x1 = lar !! 0 !! 0,
                                    y1 = lar !! 0 !! 1,
                                    x2 = lar !! 1 !! 0,
                                    y2 = lar !! 1 !! 1 }
                                    -- points = buildPoints lar }

scribedMap :: [LineSegment] -> Map
scribedMap segments = buildMap $ filter (\s -> isHoriz s || isVert s) segments
