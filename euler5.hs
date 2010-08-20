module Main where

import Euler
import NumberTheory
import Combinatorics
import Primes
import Factor
import Ratio
import Debug.Trace
import Data.List hiding (insert)
import Data.Ord
import Data.Char
import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Control.Monad
import Maybe
import Data.Array.IArray
import Foreign
import IO

--p118
oneNinePandigital x = sort (show x) == "123456789"
relevantPrimes =
    filter (\p -> nub (show p) == show p) $
    (map read $ words $ unsafePerformIO $ readFile "billprimes2.txt" :: [Int])
exclusionLists [] = replicate 9 []
exclusionLists (p:primes) =
    let digitList = show p
        nextStep = exclusionLists primes in
    map (\d -> if d `elem` digitList then
                   nextStep !! (index ('1','9') d) else
                   p : (nextStep !! (index ('1','9') d)))
            ['1'..'9']
exclusionSets primes =
    map (IS.fromDistinctAscList . reverse) (exclusionLists primes)
excl = exclusionSets relevantPrimes
partners [] = relevantPrimes
partners partialSet =
    IS.toList $
    foldr1 (IS.intersection) $
    map ((excl !!) . (index ('1','9'))) $
    nub $ concatMap show partialSet
p118 = sortNub $ map sort $ p118' []
    where p118' :: [Int] -> [[Int]]
          p118' partialSet =
              let myPartners = partners partialSet in
              if null myPartners then
                  filter ((==9) . length . concat . map show) [partialSet] else
                  concatMap p118' $ map (:partialSet) myPartners
--main = print (length p118)
--main = print (length relevantPrimes)

--p230
argument n = (127+19*n) * 7^n
--fibs' = 0:fibs

dab :: Integer -> Char
dab n =
    let m = (n+99) `div` 100
        fibTerm = 1 + length (takeWhile (<m) fibs) in
    (dab' m fibTerm) !! (fromIntegral $ (n-1) `mod` 100)
dab' 1 1 = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
dab' 1 2 = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
--dab' 1 1 = "1415926535"
--dab' 1 2 = "8979323846"
dab' m fibTerm =
    --trace (show m ++ " " ++ show fibTerm) $
    if m > fibs !! (fibTerm - 2 - 1) then
        dab' (m - (fibs !! (fibTerm - 2 - 1))) (fibTerm - 1) else
        dab' m (fibTerm - 2)
--p196
t n = n*(n+1)`div`2
trirow n = [t n - n + 1 .. t n]
rownum n = ceiling $ -0.5 + 0.5 * sqrt (1 + 8*fromIntegral n)
firstInRow :: Integer -> Bool
firstInRow n = n == t (row - 1) + 1
    where row = rownum n
lastInRow n = n == t (rownum n)
secondToLastInRow n = lastInRow (n + 1)
stack r = concatMap trirow [r-2..r+2]
primeNeighbors = filter isPrime . neighbors
neighbors n
    | firstInRow n = [n + row, n + row + 1,
                      n + 1,
                      n - row, n - row + 1]
    | lastInRow n = [n + row - 1,
                     n - 1,
                     n - row - 1, n - row, n - row + 1]
    | secondToLastInRow n = [n + row - 1, n + row,
                             n - 1, n + 1,
                             n - row - 1, n - row, n - row + 1]
    | otherwise = [n + row - 1, n + row, n + row + 1,
                   n - 1, n + 1,
                   n - row - 1, n - row, n - row + 1]
    where row = rownum n
--p165
blumBlumShub = drop 1 $
               map (`mod`500) $ iterate ((`mod`50515093) . (^2)) 290797
--p221
--alexWithQR :: Int -> [Int]
alexWithQR t = do
  q <- [-1,-2..t`div`2]
  let r = t-q
  if (q+r) `divides` (1-q*r) && (q+r) `divides` (q*r*(1-q*r)) then
      --return $ ((q*r*(1-q*r))`div`(q+r), ((1-q*r) `div` (q+r), q, r)) else
      return $ (q*r*(1-q*r))`div`(q+r) else
      fail "nope"
alex = [p * (p+d) * (p+(p*p+1)`div`d) | p <- [1..], d <- divisors (p*p+1)]
--p240

top10s = filter (all (<=12)) $ integerPartition2 10 70
--completions g = 10 arbit choices from [1..minimum g]
numMultisetPermutations g =
    let g' = group $ sort g
    in factorial (length g) `div` product (map (factorial . length) g')
pick :: Int -> Int -> [[Int]]
pick 0 _ = [[]]
pick n max = do
  x <- [1..max]
  map (x:) $ pick (n-1) (min x max)
p240' = do
  tops <- top10s
  extends <- pick 10 (minimum tops)
  return $ numMultisetPermutations (tops ++ extends)
p240 = foldl' (+) 0 p240'

--p232
getrace i j
    | i >= 100 = 1%1
    | j >= 100 = 0%1
    | otherwise = race ! (i,j)
race :: Array (Int,Int) Rational
race = listArray ((0,0), (99,99)) [computeVal i j | i <- [0..99], j <- [0..99]]
computeVal i j = maximum [expectation i j n | n <- [1..8]] :: Rational
expectation :: Int -> Int -> Int -> Rational
expectation i j n =
    let rest = ( 1        % (2^(n+1)) * getrace (i + 2^(n-1)) (j + 1)) +
               ((2^n - 1) % (2^(n+1)) * getrace (i + 0)       (j + 1)) +
               ( 1        % (2^(n+1)) * getrace (i + 2^(n-1)) (j + 0))
    in 2 * rest / (1%1 + 1%2^n)
p232 :: Rational
p232 = race ! (0,0)
--main = putStrLn $ take 20 $ rationalDigitsString p232


--p254

split254 i = sort $
           map joinDigits $
           filter ((<4) . length) $
           filter ((>1) . length) $
           integerPartition254 i

g254 i =
    let ps = split254 i
        bigList = concatMap sdFactorialPartition ps
    in
        (joinDigits . sort . minimumBy (comparing length)) bigList

integerPartition254 :: Integral b => b -> [[b]]
integerPartition254 n = integerPartition254' n n
--integerPartition' 1 n = [replicate (fromIntegral n) 1]
integerPartition254' k 0 = [[]]
integerPartition254' k n = concat [map (x:) (integerPartition254' x (n-x))
                                       | x <- [1..n]]
sdFactorialPartition n = sdFactorialPartition' 9 n
sdFactorialPartition' 1 n = [replicate (fromIntegral n) 1]
sdFactorialPartition' d 1 = [[]]
sdFactorialPartition' d n = concat [map (x:) $ filter ((<3) . length) $ (sdFactorialPartition' x (n - factorial x))
                                        | x <- [0 .. d], factorial x <= n]

--p229
isSumOfTwoSquares n =
    all (even . length) $ group $ filter ((==3) . (`mod` 4)) $ factor n
is229Worthy n =
    isSumOfTwoSquares n &&
    isSumOfSpecialSquares 2 n &&
    isSumOfSpecialSquares 3 n &&
    isSumOfSpecialSquares 7 n
isSumOfSpecialSquares r n =
    any (\b -> isSquare $ n - r*b*b) [1..floor (sqrt $ fromIntegral n / fromIntegral r)]
p229 = length $ filter is229Worthy [1..10^6]
main = print p229
