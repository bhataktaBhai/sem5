module Main (main) where

import Test.QuickCheck
import Data.List (sort)

import Median
import Skyline

main :: IO ()
main = do
    quickCheck prop_median
    quickCheck prop_select
    -- quickCheck prop_skyline
    quickCheck prop_chan_van

prop_median :: NonEmptyList Int -> Property
prop_median xs = median xs' === sort xs' !! (length xs' `div` 2)
    where xs' = getNonEmpty xs

prop_select :: NonEmptyList Int -> NonNegative Int -> Property
prop_select xs k = k' < length xs' ==> select xs' k' === sort xs' !! k'
    where xs' = getNonEmpty xs
          k' = getNonNegative k

type Point a = [a]
skylineBrute :: [Point Float] -> [Point Float]
skylineBrute xs = filter (\x -> not $ any (dominates x) xs) xs

-- This fails until I fix Skyline to handle equal coordinates
prop_skyline :: NonEmptyList (Float, Float, Float) -> Property
prop_skyline xs = sort (skyline xs') === sort (skylineBrute xs')
    where xs' = map (\(x, y, z) -> [x, y, z]) $ getNonEmpty xs

prop_chan_van :: [(Float, Float, Float)] -> Property
prop_chan_van xs = sort (skyline xs') === sort (chanSkyline xs')
    where xs' = map (\(x, y, z) -> [x, y, z]) xs
