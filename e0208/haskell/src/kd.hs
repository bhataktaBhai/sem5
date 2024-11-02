type Rect a = (Int, [(a, a)])
type Point a = (Int, [a])

data KdTree a = Leaf (Point a) | Split a (KdTree a) (KdTree a) deriving Show

kdTree :: Ord a => [Point a] -> KdTree a
kdTree [] = error "No concept of empty tree"
kdTree [x] = Leaf x
kdTree xs = splitHalf xs

-- queryKd :: KdTree Int -> Rect2D -> [Point2D]
-- queryKd (Leaf (x, y)) Rect2D {l=l, r=r, d=d, u=u}
--   = [(x, y) | l <= x && x <= r && d <= y && y <= u]
-- queryKd (SplitX x l r) (x1, x2) (y1, y2)
--   | x1 > x    = queryKd r (x1, x2) (y1, y2)
--   | x2 < x    = queryKd l (x1, x2) (y1, y2)
--   | otherwise = queryKd l (x1, x2) (y1, y2) ++ queryKd r (x1, x2) (y1, y2)
