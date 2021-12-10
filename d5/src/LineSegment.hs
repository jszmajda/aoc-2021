module LineSegment (
  LineSegment(..), Point, isOnLineSegment, isHoriz, isVert
) where
type Point = (Int, Int)
data LineSegment = LineSegment {
  x1 :: Int,
  y1 :: Int,
  x2 :: Int,
  y2 :: Int
  -- points :: [Point]
} deriving (Eq)

instance Show LineSegment where
  show ls = "(" ++ (show (x1 ls)) ++ "," ++ (show (y1 ls)) ++ ") -> (" ++ (show (x2 ls)) ++ "," ++ (show (y2 ls)) ++ ")"

--buildPoints :: [[Int]] -> [Point]
--buildPoints [[lx1, ly1], [lx2, ly2]] = foldl' yloop [] yrange
--  where
--    yloop :: [Point] -> Int -> [Point]
--    yloop acc y = snd $ (foldl' xloop (y, acc) xrange)
--    xloop :: (Int, [Point]) -> Int -> (Int, [Point])
--    xloop (y, acc) x = (y, ((x, y) : acc))
--    xrange = if (lx1 > lx2) then [lx2..lx1] else [lx1..lx2]
--    yrange = if (ly1 > ly2) then [ly2..ly1] else [ly1..ly2]

isOnLineSegment :: Point -> LineSegment -> Bool
isOnLineSegment p ls = if crossproduct > 0 then False else (if dotproduct < 0 then False else (if dotproduct > squaredlengthba then False else True))
  where
    crossproduct = abs $ (y - (y1 ls)) * ((x2 ls) - (x1 ls)) - (x - (x1 ls)) * ((y2 ls) - (y1 ls))
    dotproduct = (x - (x1 ls)) * ((x2 ls) - (x1 ls)) + (y - (y1 ls)) * ((y2 ls) - (y1 ls))
    squaredlengthba = ((x2 ls) - (x1 ls)) * ((x2 ls) - (x1 ls)) + ((y2 ls) - (y1 ls)) * ((y2 ls) - (y1 ls))
    x = fst p
    y = snd p

-- def isBetween(a, b, c):
--     crossproduct = (c.y - a.y) * (b.x - a.x) - (c.x - a.x) * (b.y - a.y)
-- 
--     if abs(crossproduct) > 0:
--         return False
-- 
--     dotproduct = (c.x - a.x) * (b.x - a.x) + (c.y - a.y)*(b.y - a.y)
--     if dotproduct < 0:
--         return False
-- 
--     squaredlengthba = (b.x - a.x)*(b.x - a.x) + (b.y - a.y)*(b.y - a.y)
--     if dotproduct > squaredlengthba:
--         return False
-- 
--     return True

isHoriz :: LineSegment -> Bool
isHoriz ls = (x1 ls) == (x2 ls)

isVert :: LineSegment -> Bool
isVert ls = (y1 ls) == (y2 ls)

