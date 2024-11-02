module Skyline (skyline, dominates, chanSkyline) where

import Median (medianBy)
import Data.List (sort, sortBy, partition)
import Data.Ord (comparing)
type Point a = [a]

data SkyTree a = Leaf { point :: Point a
                      , minX  :: a
                      , maxX  :: a
                      }
                 | Branch { key   :: a
                          , minX  :: a
                          , maxX  :: a
                          , lefT  :: SkyTree a
                          , righT :: SkyTree a
                          } deriving (Show)

inf :: Float
inf = 1/0

-- sorted by x, left-biased
treeFrom :: [Point Float] -> SkyTree Float
treeFrom = treeFromSorted . sort
    where
    treeFromSorted :: [Point Float] -> SkyTree Float
    treeFromSorted [p] = Leaf { point = p, minX = inf, maxX = -inf }
    treeFromSorted ps  = Branch { key = head $ head rgtPs
                          , minX = inf
                          , maxX = -inf
                          , lefT = treeFrom lftPs
                          , righT = treeFrom rgtPs
                          } where (lftPs, rgtPs) = splitAt ((length ps - 1) `div` 2) ps

branchFrom :: Float -> SkyTree Float -> SkyTree Float -> SkyTree Float
branchFrom k lft rgt = Branch { key = k
                              , minX = min (minX lft) (minX rgt)
                              , maxX = max (maxX lft) (maxX rgt)
                              , lefT = lft
                              , righT = rgt
                              }

turnOn, turnOff :: Point Float -> SkyTree Float -> SkyTree Float
turnOn p (Leaf {point = p'})
  | p' == p   = Leaf p' (head p') (head p')
  | otherwise = error "Point not found"
turnOn p (Branch {key = k, lefT = lft, righT = rgt})
  | head p <= k = branchFrom k (turnOn p lft) rgt
  | otherwise   = branchFrom k lft (turnOn p rgt)

turnOff p (Leaf {point = p'})
  | p' == p   = Leaf p' inf (-inf)
  | otherwise = error "Point not found"
turnOff p (Branch {key = k, lefT = lft, righT = rgt})
  | head p <= k = branchFrom k (turnOff p lft) rgt
  | otherwise   = branchFrom k lft (turnOff p rgt)

succTree :: Point Float -> SkyTree Float -> Point Float
succTree p (Leaf {point = p', maxX = mx} )
  | mx > head p = p'
  | otherwise   = [inf, -inf, -inf]
succTree p (Branch {lefT = lft, righT = rgt})
  | head p < maxX lft = succTree p lft
  | head p < maxX rgt = succTree p rgt
  | otherwise = [inf, -inf, -inf]

predTree :: Point Float -> SkyTree Float -> Point Float
predTree p (Leaf {point = p', minX = mn} )
  | mn < head p = p'
  | otherwise   = [-inf, inf, -inf]
predTree p (Branch {lefT = lft, righT = rgt})
  | head p > minX rgt = predTree p rgt
  | head p > minX lft = predTree p lft
  | otherwise = [-inf, inf, -inf]

trimSkyline :: Point Float -> SkyTree Float -> SkyTree Float
trimSkyline p t
  | p' !! 1 < p !! 1 = trimSkyline p $ turnOff p' t
  | otherwise        = t
    where p' = predTree p t

addToSkyline :: Point Float -> SkyTree Float -> SkyTree Float
addToSkyline p = trimSkyline p . turnOn p

skyline :: [Point Float] -> [Point Float]
skyline ps = skyline' (reverse sortByZ) (treeFrom ps)
  where
    sortByZ = sortBy (comparing (!! 2)) ps

    skyline' :: [Point Float] -> SkyTree Float -> [Point Float]
    skyline' [] _ = []
    skyline' (q:qs) t
      | q !! 1 >= q' !! 1 = q : skyline' qs (addToSkyline q t)
      | otherwise         = skyline' qs t
          where q' = succTree q t

-- random points:
-- skyline [[1, 14, 28], [2, 6, 7], [3, 13, 9], [12, 7, 16], [14, 3, 25], [19, 18, 22], [23, 13, 29], [24, 4, 27]]

-- Do NOT use these as infix operators.
-- p `dominates` q means q dominates p.
-- The intention is that (dominates p) is a predicate declaring a point to
-- be dominating p.
dominates, dominatedBy :: Point Float -> Point Float -> Bool
dominates p q = and $ zipWith (<) p q
dominatedBy = flip dominates

limitedSkyline :: Int -> [Point Float] -> SkyTree Float -> [Point Float]
limitedSkyline _ [] _ = []
limitedSkyline k ps t = limitedSkyline' k filtered
    where filtered = filter (\p -> succTree p t !! 1 <= p !! 1) ps
          limitedSkyline' :: Int -> [Point Float] -> [Point Float]
          limitedSkyline' 0 _  = []
          limitedSkyline' _ [] = []
          limitedSkyline' k ps = p : limitedSkyline' (k - 1) ps'
              where p = maximum ps
                    ps' = (filter (/= p) . filter (not . dominatedBy p)) ps

partitionBy :: (a -> a -> Ordering) -> Int -> [a] -> [[a]]
partitionBy _ _ [] = []
partitionBy _ 0 xs = [xs]
partitionBy cmp depth xs = let m = medianBy cmp xs
                               (xls, xrs) = partition (\x -> x `cmp` m /= GT) xs
                            in partitionBy cmp (depth - 1) xls ++ partitionBy cmp (depth - 1) xrs

chan :: Int -> [Point Float] -> Maybe [Point Float]
chan _ [] = Just []
chan d ps = let parts = reverse $ partitionBy (comparing (!! 2)) d ps in
                chan' (2^d) parts (treeFrom ps)
    where chan' :: Int -> [[Point Float]] -> SkyTree Float -> Maybe [Point Float]
          chan' _ [] _ = Just []
          chan' k (part:parts) t
            | length partSky == k + 1 = Nothing
            | otherwise = do
                restSky <- chan' (k - length partSky) parts t'
                return $ partSky ++ restSky
            where partSky = limitedSkyline (k + 1) part t
                  t' = foldr addToSkyline t partSky

chanSkyline :: [Point Float] -> [Point Float]
chanSkyline = chanSkyline' 2
    where
        chanSkyline' :: Int -> [Point Float] -> [Point Float]
        chanSkyline' d ps = case chan d ps of
            Just sky -> sky
            Nothing  -> chanSkyline' (2 * d) ps
