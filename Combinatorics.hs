module Combinatorics
    where

import Data.List

factorial n = product [2..n]
nPr n k = product [k+1..n]
nCr n k = product [k+1..n] `div` (factorial (n-k))

{- GHC 6.10 includes permutations in Data.List
permutations :: [a] -> [[a]]
permutations [a] = [[a]]
permutations x = concatMap (\i -> map (x!!i:)
                            (permutations $ (take i x) ++ (drop (i+1) x)))
                 [0..length x - 1]
-}
choose :: Int -> [a] -> [[a]]
choose 0 as = [[]]
choose n as
    | length as < n = []
    | length as == n = [as]
    | otherwise = (map (head as:) $ choose (n-1) (tail as)) ++ choose n (tail as)

powerset set = concatMap ((flip choose) set) [0..length set]

unrank :: (Eq a) => [a] -> Int -> [a]
unrank [x] _ = [x]
unrank objects rank = x : unrank (delete x objects) (rank `mod` b)
    where x = objects !! (rank `div` b)
          b = factorial $ length objects - 1

countLess x objects = length $ filter (<x) objects
rank :: (Ord a) => [a] -> Int
rank [] = 0
rank [x] = 0
rank (x:objects) = b * countLess x objects + rank objects
    where b = factorial $ length objects
