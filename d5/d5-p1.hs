import qualified Data.Text as T

main = do
  contents <- lines <$> readFile "input.txt"
  print $ map parseLineSegment contents
  print "ok"

data LineSegment = LineSegment {
  x1 :: Int,
  y1 :: Int,
  x2 :: Int,
  y2 :: Int
} deriving (Show, Eq)

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
