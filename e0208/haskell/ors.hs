data PST a b = Null | Node { key :: a -- median
                           , val :: b -- max y, priority
                           , left :: PST a b
                           , right :: PST a b
                           , parent :: PST a b
                           } deriving (Eq, Show)

data Rect2D = Rect2D { l :: Int, r :: Int, d :: Int, u :: Int }
type Point2D = (Int, Int)

leftTrees :: (Ord a, Ord b) => a -> PST a b -> [PST a b]
leftTrees _ Null = []
leftTrees x n
  | key n >= x = right n : leftTrees x (left n)
  | key n < x  = leftTrees x (right n)

rightTrees :: (Ord a, Ord b) => a -> PST a b -> [PST a b]
rightTrees _ Null = []
rightTrees x n
  | key n <= x = left n : rightTrees x (right n)
  | key n > x  = rightTrees x (left n)

getAbove :: (Ord a, Ord b) => b -> PST a b -> [PST a b]
getAbove _ Null = []
getAbove y n
  | val n <  y = []
  | val n >= y = n : (getAbove y (left n) ++ getAbove y (right n))

-- lowest common ancestor
lca :: (Ord a, Ord b) => PST a b -> PST a b -> PST a b
lca m n
  | val m < val n  = lca (parent m) n
  | val n < val m  = lca m (parent n)
  | val m == val n = if m == n then m else
                            error "different nodes with same value"

-- inRectangle :: (Ord a, Ord b) => (x1, x2)
-- scanPath :: (Ord a, Ord b) => (a, a) -> PST a b

data KdTree a = Leaf (a, a) | SplitX a (KdTree a) (KdTree a)
                            | SplitY a (KdTree a) (KdTree a) deriving (Show)

queryKd :: KdTree Int -> Rect2D -> [Point2D]
queryKd (Leaf (x, y)) Rect2D {l=l, r=r, d=d, u=u}
  = [(x, y) | l <= x && x <= r && d <= y && y <= u]
-- queryKd (SplitX x l r) (x1, x2) (y1, y2)
--   | x1 > x    = queryKd r (x1, x2) (y1, y2)
--   | x2 < x    = queryKd l (x1, x2) (y1, y2)
--   | otherwise = queryKd l (x1, x2) (y1, y2) ++ queryKd r (x1, x2) (y1, y2)
