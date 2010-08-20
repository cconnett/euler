{-# LANGUAGE NoMonomorphismRestriction #-}
module Euler
    where

import Primes
import Factor
import Combinatorics
import Control.Monad
import Control.Arrow
import Data.List
import Data.Ord
import Data.Ratio
import Data.Char (isSpace)
import qualified Data.Set as S
import Debug.Trace

traceIt it = trace (show it) it
traceN k n = if n `mod` k == 0 then trace ("n = " ++ show n) n else n

fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

palindromic n = (show n) == reverse (show n)

digits :: Integral k => k -> [k]
digits = reverse.digits'
digits' n
    | n < 10 = [n]
    | otherwise = (n `mod` 10):(digits' (n `div` 10))

digitalSum :: Integral k => k -> k
digitalSum = sum . digits

joinDigits :: (Num a) => [a] -> a
joinDigits [] = 0
joinDigits ds = 10*(joinDigits (init ds)) + (last ds)

evens [] = []
evens [e] = [e]
evens (e:o:rest) = e : evens rest
odds [] = []
odds [e] = []
odds (e:o:rest) = o : odds rest

integerPartition :: Integral b => b -> [[b]]
integerPartition n = integerPartition' n n
integerPartition' 1 n = [replicate (fromIntegral n) 1]
integerPartition' k 0 = [[]]
integerPartition' k n = concat [map (x:) (integerPartition' x (n-x))
                                    | x <- [1..max k n]]

integerPartition2 :: Integral b => b -> b -> [[b]]
integerPartition2 k n = integerPartition2' n k n
integerPartition2' max k n
    | k > n = []
integerPartition2' max 1 n
    | n > max = []
    | otherwise = [[n]]
integerPartition2' max k n = concat [map (x:) (integerPartition2' x (k-1) (n-x))
                                     | x <- [1..min max (n-1)]]

countPartitions m = numPartitions !! m
numPartitions :: [Int]
numPartitions = map ((`mod`1000000) . numPartitions') [0..]
numPartitions' 1 = 1
numPartitions' m = sum [alt (j+1) * countPartitions (m - (3*j^2 - j) `div` 2)
                            | j <- [1..2*m], 3*j^2 - j < 2*m]
                   +
                   sum [alt (j+1) * countPartitions (m - (3*j^2 + j) `div` 2)
                            | j <- [1..2*m], 3*j^2 + j < 2*m]
alt n
    | even n = 1
    | odd n = -1

intersperseAll as [] = [as]
intersperseAll [] bs = [bs]
intersperseAll (a:as) (b:bs)
    | a == b = map (\li -> a:b:li) (intersperseAll as bs)
    | otherwise = concat [map (a:) (intersperseAll as (b:bs)),
                          map (b:) (intersperseAll (a:as) bs)]

rad = product . nub . factor

sortNub = S.toList . S.fromList

-- Find the first item in the given search space that satisfies the
-- predicate p, by unbounded binary search.  Search space may be
-- infinite.  The predicate must be unsatisfiable up to some point in
-- the search space, after which all elements satisfy it.  This
-- function is intended to be used when the predicate is expensive.
findFirst :: MonadPlus m => (a -> Bool) -> [a] -> m a
findFirst p space = liftM (space !!) answer
    where answer = findFirst' (map p space) 0
findFirst' cache step
    | null cache = mzero
    | head cache = return 0
    | (not $ cache `hasIndex` point) || cache !! point =
        liftM (ps +) $ findFirst' (drop ps cache) 0
    | otherwise = findFirst' cache (next step)
    where point = step
          ps = prev step

next 0 = 1
next s = 2*s
prev 1 = 1
prev s = s `div` 2

hasIndex [] c = False
hasIndex list 0 = True
hasIndex list c = (tail list) `hasIndex` (c-1)

mixed rat = second (%denominator rat) $
            divMod (numerator rat) (denominator rat)

-- Individual digits after the decimal of the expansion of the given
-- rational number computed in base 10.
fractionalDigits ratio =
    (replicate extraZeroes 0) ++ (digits $ biggerNumerator `div` denom) ++
    if biggerNumerator `mod` denom == 0 then [] else
    (fractionalDigits $ (biggerNumerator `mod` denom) % denom)
    where (biggerNumerator, extraZeroes) =
              (\(l, r) -> (head r, length l - 1)) (span (<denom) (iterate (*10) numer))
          numer = numerator ratio
          denom = denominator ratio

rationalDigitsString ratio =
    show (numerator ratio `div` denominator ratio) ++ "." ++
    concatMap show (fractionalDigits ratio)

isSquare n = (floor $ sqrt (fromInteger n))^2 == n

-- Rational convergents to square roots
rationalConvergents :: Integer -> [Rational]
rationalConvergents n
    | w^2 == n = repeat (w%1)
    | otherwise = computeContinuedFractions $
                  continuedFractionCoefficients (n, 0, 1)
    where w = floor $ sqrt (fromIntegral n)

computeContinuedFractions :: Integral b => [b] -> [Ratio b]
computeContinuedFractions [] = []
computeContinuedFractions (coeff:coeffs) =
    (coeff % 1) :
    (map ( ((coeff%1)+) . (1 / ) )
         (computeContinuedFractions coeffs))

continuedFractionCoefficients :: (Integer, Integer, Integer) -> [Integer]
continuedFractionCoefficients arg@(n, addend, divisor) =
    --trace (show arg) $
    a_i : continuedFractionCoefficients (n, -addend', (n - addend'^2)`div`divisor)
    where a_i = floor $ ((sqrt $ fromIntegral n) + (fromIntegral addend)) / (fromIntegral divisor)
          addend' = addend - a_i*divisor


takeEvery :: Int -> [a] -> [a]
takeEvery k [] = []
takeEvery k l = head l : takeEvery k (drop k l)

deinterlace k l = map (takeEvery k) (take k (tails l))

diffs seq = zipWith (-) (tail seq) seq

trim = f . f
    where f = reverse . dropWhile isSpace
tally x = --trace (show x) $
    sortBy (comparing snd) $
    map (\g -> (head g, length g)) $ group $ sort x
selectDigit d n = n `mod` 10^d `div` 10^(d-1)

--p148 [Wed Sep 12 2007 / 21:21 EDT]
pascalsTriangle :: [[Integer]]
pascalsTriangle = [1] : (map nextRow pascalsTriangle)
    where nextRow row = concat [[1], (zipWith (+) row (tail row)), [1]]

getTriple m n = (2*m*n, m*m - n*n, m*m + n*n)
primitivePythagoreanTriples =
    [getTriple m n | m <- [1..], n <- [1..m],
     gcd m n == 1, odd m && even n || even m && odd n]
