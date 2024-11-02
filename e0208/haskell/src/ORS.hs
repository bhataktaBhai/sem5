{- | Orthogonal range searching

    * Three sided range queries in 2D using priority search trees

        * Top-k reporting in 1D
        * Colored range reporting in 1D

    * Orthogonal range queries in d-dimensions using kd-trees
    * Orthogonal range queries in d-dimensions using range trees
-}
module ORS () where

import Median (medianBy)
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy)

-- * Three sided range queries in 2D using priority search trees

{- | A three sided rectangle.
    (x1, x2, y1) represents the range [x1, x2] × [y1, ∞]
-}
type Rect3 a b = (a, a, b)

-- | A 2-dimensional point
type Point2 a b = (a, b)

{- | A priority search tree.
    Stores points of type (a, b) where the first coordinate is
    the key and the second is the priority.
    Only makes sense if a and b are instances of `Ord`.

    Each node stores the point with the highest priority as `val`,
    the median of the keys of the points in the subtree as `key`,
    and the left and right subtrees.
    The left subtree contains points with keys less than or equal to the median,
    and the right subtree contains points with keys greater than the median.

    This allows for efficient range queries in 2D space.
-}
data PST a b = Null
    | Node { key :: a -- ^ The median of keys, used as splitting value
           , val :: Point2 a b -- ^ The highest priority point
           , lefT :: PST a b -- ^ Left subtree: containing (x, y) with x <= key
           , righT :: PST a b -- ^ Right subtree: containing (x, y) with x > key
           }

instance (Eq a, Eq b) => Eq (PST a b) where
    Null == Null = True
    Node {val=v} == Node {val=v'} = v == v'
    _ == _ = False

-- | Pretty printing for the tree. Copied from `Data.Tree`
draw :: (Show a, Show b, Eq a, Eq b) => PST a b -> [String]
draw Null = []
draw (Node k v l r) = (show v ++ " split by " ++ show k) : drawSubTrees (filter (/= Null) [l, r])
  where drawSubTrees [] = []
        drawSubTrees [t] =
            shift "└─ " "   " (draw t)
        drawSubTrees (t:ts) =
            shift "├─ " "│  " (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

instance (Show a, Show b, Eq a, Eq b) => Show (PST a b) where show = unlines . draw

-- | Two PSTs are equal if they store the same point at the root.
-- This is to avoid O(n) comparisons when comparing two PSTs _built from the same list_.
{- | Creates a priority search tree from a list of points.
    Assumes that the list does not contain duplicates (removes them).
-}
pstFrom :: (Ord a, Ord b) => [Point2 a b] -> PST a b
pstFrom = pstFromSorted . sortBy (comparing fst)
    where
        pstFromSorted :: (Ord a, Ord b) => [Point2 a b] -> PST a b
        pstFromSorted [] = Null
        pstFromSorted ps = Node { key = k
                                , val = v
                                , lefT = pstFromSorted lefts
                                , righT = pstFromSorted rights
                                }
            where v = maximumBy (comparing snd) ps
                  ps' = filter (/= v) ps
                  k = if null ps' then fst v else map fst ps' !! ((length ps' - 1) `div` 2)
                  lefts = takeWhile ((<= k) . fst) ps'
                  rights = dropWhile ((> k) . fst) ps'

-- | Searches for x1 and x2 in the PST T, and returns the common path
-- from the root to the 'split node'.
commonPath :: Ord a => a -- ^ x1
                       -> a -- ^ x2
                       -> PST a b -- ^ T
                       -> [PST a b] -- ^ common search path
commonPath _ _ Null = []
commonPath x1 x2 n = n : cmp
    where cmp | b <= key n = commonPath a b (lefT n)
              | a >  key n = commonPath a b (righT n)
              | otherwise  = []
          (a, b) = (min x1 x2, max x1 x2)

-- | The search path for x starting from node n.
path :: Ord a => a {-^x-} -> PST a b {-^n-} -> [PST a b] {-^Search path from `n`-}
path _ Null = []
path x n
  | x < key n = n : path x (lefT n)
  | x > key n = n : path x (righT n)
  | otherwise = [n]

-- | Given path Π, report the list of left/right children
-- of nodes in Π, if they don't themselves lie in Π.
canonical :: (Ord a, Ord b) =>
    -- | `lefT` or `righT`
    (PST a b -> PST a b) ->
    -- | Π
    [PST a b] ->
    -- | left or right canonical subtrees
    [PST a b]
canonical _ [] = []
canonical f [n] = [f n | f n /= Null]
canonical f (n:n':ns) = [f n | f n /= n'] ++ canonical f (n':ns)

-- | Given path Π, report the list of right children
-- of nodes in Π, if they don't themselves lie in Π.
rightCanonical :: (Ord a, Ord b) => [PST a b] {-^Π-} -> [PST a b] {-^Right canonical subtrees-}
rightCanonical = canonical righT

-- | Given path Π, report the list of left children
-- of nodes in Π, if they don't themselves lie in Π.
leftCanonical :: (Ord a, Ord b) => [PST a b] {-^Π-} -> [PST a b] {-^Left canonical subtrees-}
leftCanonical = canonical lefT

-- | Given a 3-sided rectangle and a point, check if the point lies in the rectangle.
in3Sided :: (Ord a, Ord b) => Rect3 a b -> Point2 a b -> Bool
in3Sided (x1, x2, y1) (x, y) = x1 <= x && x <= x2 && y1 <= y

-- | Given a 3-sided rectangle and the root of a canonical subtree, report all points in the subtree
-- that lie in the rectangle.
reportCanonical :: (Ord a, Ord b) => Rect3 a b -> PST a b -> [Point2 a b]
reportCanonical _ Null = []
reportCanonical r n
  | in3Sided r (val n) = val n : reportCanonical r (lefT n)
                              ++ reportCanonical r (righT n)
  | otherwise = []

-- | Given a 3-sided rectangle and a PST, report all points in the PST that lie in the rectangle.
report :: (Ord a, Ord b) => Rect3 a b -> PST a b -> [Point2 a b]
report r t = let (x1, x2, _) = r
                 cp = commonPath x1 x2 t
                 sp = last cp
                 lft = tail $ path x1 sp
                 rgt = tail $ path x2 sp
    in reportFrom cp ++ reportFrom lft ++ reportFrom rgt
        ++ concatMap (reportCanonical r) (rightCanonical lft)
        ++ concatMap (reportCanonical r) (leftCanonical rgt)
        where reportFrom = filter (in3Sided r) . map val

type Point a = [a]
type Rect a = [(a, a)]
data KdT a = Leaf (Point a) | Branch Int (KdT a) (KdT a) deriving (Show)

uncycle :: Int -> Int -> [a] -> [a]
uncycle d i = take d . drop (d - i)

kdtFrom :: Ord a => Int {-^dimension-} -> [Point a] -> KdT a
kdtFrom d = kdtFrom' 0 . map cycle
    where
        kdtFrom' :: Ord a => Int -> [Point a] -> KdT a
        kdtFrom' i [p] = Leaf (uncycle d i p)
        kdtFrom' i ps = Branch i (kdtFrom' i' lefts) (kdtFrom' i' rights)
            where i' = (i + 1) `mod` d
                  med = medianBy (comparing head) ps
                  lefts = map tail $ filter (<= med) ps
                  rights = map tail $ filter (> med) ps

inRect :: Ord a => Rect a -> Point a -> Bool
inRect rect p = and $ zipWith (\(x1, x2) x -> x1 <= x && x <= x2) rect p

queryKdT :: (Ord a) => KdT a -> Rect a -> [Point a]
queryKdT (Leaf p) rect = [p | inRect rect p]
queryKdT (Branch i l r) rect = queryKdT l rect' ++ queryKdT r rect'
    where rect' = tail rect
