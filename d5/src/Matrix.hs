{-# LANGUAGE BangPatterns #-}

module Matrix (
  Matrix, getElem, zero, nrows, ncols, pretty, sumApplyPoints, buildMap, toList
) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import qualified Data.List as L
import           Data.List (foldl')
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import LineSegment
--import Debug.Trace

data Matrix = M {
  nrows :: !Int,
  ncols :: !Int,
  mdata :: V.Vector Int
}

slice :: Int -> Int -> [a] -> [a]
slice from n list = take n $ drop from list

pretty :: Matrix -> String
pretty m = unlines $ map prettyRow [0..((nrows m) - 1)]
  where
    vl = V.toList v
    prettyRow rn = unwords $ map prettyCol (slice (rn * w) w vl)
    prettyCol val = show val
    w = ncols m
    v = mdata m

-- row, column, matrix
getElem :: Int -> Int -> Matrix -> Int
getElem row col m = (mdata m) ! (xyToOffset m (row, col))

xyToOffset :: Matrix -> (Int, Int) -> Int
xyToOffset m p = (h * y) + x
  where
    x = fst p
    y =  snd p
    h = nrows m

updatedMatrix :: Matrix -> V.Vector Int -> Matrix
updatedMatrix m v = M (nrows m) (ncols m) v

sumApplyPoints :: Matrix -> [(Int, Int)] -> Matrix
sumApplyPoints m points = runST $ do
  v <- V.thaw (mdata m)
  mapM_ (applyPoint v) points
  finalVec <- V.freeze v
  return $ updatedMatrix m finalVec
  where
    applyPoint vec point = do
      oldVal <- MV.read vec (ofs point)
      MV.write vec (ofs point) (oldVal + 1)
      return ()
    ofs point = xyToOffset m point

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

buildMap :: [LineSegment] -> Matrix
buildMap segments = runST $ do
  let z = empty
  v <- V.thaw (mdata z)
  mapM_ (applySegmentsAtPoint z v segments) (pointSet z)
  finalVec <- V.freeze v
  return $ updatedMatrix z finalVec
  where
    applySegmentsAtPoint :: Matrix -> MV.MVector s Int -> [LineSegment] -> (Int, Int) -> ST s ()
    applySegmentsAtPoint z vec segs p = do
      let intersects = count (\ls -> isOnLineSegment p ls) segs
      --let offset = traceShow ("offset: " ++ (show p) ++ " width: " ++ (show (ncols z)) ++ " height: " ++ (show (nrows z)) ++ " to " ++ (show (xyToOffset z p)) ++ " for " ++ (show (MV.length vec))) $ xyToOffset z p
      let offset = xyToOffset z p
      MV.write vec offset intersects
      return ()
    pointSet :: Matrix -> [(Int, Int)]
    pointSet x = foldl' (\acc row -> foldl' (\zacc col -> (col, row) : zacc) acc [0..(ncols x)]) [] [0..(nrows x)]
    empty = zero rows cols
    rows = snd largestXY
    cols = fst largestXY
    largestXY = foldl' (\(mx, my) ls -> (if (x1 ls > mx) then (x1 ls) else if (x2 ls > mx) then (x2 ls) else mx,
                                        if (y1 ls > my) then (y1 ls) else if (y2 ls > my) then (y2 ls) else my)
                       ) (0,0) segments

-- Rows, Cols
zero :: Int -> Int -> Matrix
zero rows cols = M rows cols $ V.replicate ((rows * rows) + cols + 1) 0

toList :: Matrix -> [Int]
toList m = V.toList $ mdata m
