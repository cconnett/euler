module Main where

import Primes
import Factor
import NumberTheory
import Combinatorics
import Euler
import Data.List
import Foreign (unsafePerformIO)
import Control.Arrow
import Maybe
import Data.Ratio
import Data.Ord
import qualified Data.Set as S
import Test.QuickCheck
import Debug.Trace
import AStar

--p160
million = 10^6
billion = 10^9
trillion = 10^12

count p n = count' p p n
count' p d n
    | d > n = 0
    | otherwise = n `div` d + (count' p (d*p) n)

--p160 :: ModInt
--p160 = 2^(countt 2 - countt 5) * 3^(countt 3) * foldl1 (*) (map (\p -> p^(countt p)) $ filter (<=trillion) primes)
--       where countt p = count p trillion

--p43 [Mon Sep 17 2007 / 01:13 EDT]
d3d n = filter ((==0) . (`mod`n)) [1..999]


--p46 [Mon Sep 17 2007 / 00:11 EDT]
oddComposites = filter (not.isPrime) [35,37..]
sansSquares :: [(Int, [Int])]
sansSquares = map (\n -> (n, takeWhile (>0) (map ((n-).(*2).(^2)) [1..]))) oddComposites
p46 = fst $ head $ filter (not . (any isPrime) . snd) sansSquares
-- Solved [Mon Sep 17 2007 / 00:32 EDT]

--p49 [Mon Sep 17 2007 / 00:48 EDT]
p4d = filter isPrime [1000..9999]
permOf a b = (sort $ show a) == (sort $ show b)
p49 = [(a,b,c)
           | a <- p4d,
             b <- filter (permOf a) $ filter (>a) p4d,
             c <- filter (permOf a) $ filter (`elem`p4d) [2*b-a]]

--p102 [Sun Sep 23 2007 / 04:03 EDT]
triangles = map parseTri $ lines $ unsafePerformIO $ readFile "triangles.txt"
parseTri :: String -> [(Int, Int)]
parseTri line = mkTuples $ read ("["++line++"]")
    where mkTuples [] = []
          mkTuples (x:y:xs) = (x,y):(mkTuples xs)
vectors triangle = zipWith (\(a,b) (c,d) -> (c-a,d-b)) triangle (tail $ cycle triangle)
crossProduct (ux,uy) (vx,vy) = ux*vy-uy*vx
sign num
    | num < 0 = -1
    | num > 0 = 1
    | num == 0 = 0
originSide (px, py) (vx, vy) = sign $ crossProduct (-px, -py) (vx, vy)
containsOrigin triangle =
    all ((==originSide (head triangle) (head $ vectors triangle)) .
         (uncurry originSide)) (zip triangle (vectors triangle))
p102 = length $ filter containsOrigin triangles
-- Solved [Sun Sep 23 2007 / 04:22 EDT]


--p50 [Sun Sep 23 2007 / 04:22 EDT]
longestConsecutivePrimeChainSub n p = last $
    takeWhile (\(i,x) -> x < n) $ filter (\(i,x) -> isPrime x) $
    zip [1..] $ scanl1 (+) (dropWhile (<p) primes)
p50 = maximum $ map (longestConsecutivePrimeChainSub 1000000) (takeWhile (<1000000) primes)
-- [Sun Sep 23 2007 / 15:51 EDT]

--p87
sqs = S.fromList $ map (^2) $ filter isPrime [1..7071]
myx = filter remCubeIsSquare [1..50000000-1]
sqCbs = S.fromList $ myx
remCubeIsSquare n = any (\cb -> (n - cb) `S.member` sqs) $ map (^3) $ filter isPrime [1..368]
scfs = filter remFourthIsSqCb [1..50000000-1]
remFourthIsSqCb :: Int -> Bool
remFourthIsSqCb n = any (\fourth -> (n - fourth) `S.member` sqCbs) $ map (^4) $ filter isPrime [1..84]
p87 = putStrLn $ show $ length scfs

--romanNums
romanize n
    | n >= 1000 = "M" ++ romanize (n-1000)
    | n >= 500 = "D" ++ romanize (n-500)
    | n >= 100 = "C" ++ romanize (n-100)
    | n >= 50 = "L" ++ romanize (n-50)
    | n >= 10 = "X" ++ romanize (n-10)
    | n >= 5 = "V" ++ romanize (n-5)
    | n >= 1 = "I" ++ romanize (n-1)
    | n == 0 = ""
    | otherwise = error "bad n"

--p149 [Thu Nov 15 2007 / 16:53 EST]
{-lfg k
    | 1 <= k <= 55 = (100003-200003*k+300007*k^3)`mod`1000000 - 500000
    | otherwise =
-}

--p108 [Mon Nov 26 2007 / 00:39 EST]
diophantineSolns n = zip r (map (denominator . (1%n-) . (1%)) r)
    where r = filter (existsY . (1%n-) . (1%)) [n+1..2*n]
existsY = (==1).numerator
upperBoundNDS n = (length $ divisors n) ^ 2
numDiophantineSolns n = --trace (show n) $
    length [True | a <- divisorsN, b <- dropWhile (<a) divisorsN,
                   relPrime a b]
        where divisorsN = divisors n
p108 start x = head $ filter ((>x) . numDiophantineSolns) $
               filter ((>x) . upperBoundNDS) hcn
--main = print $ (it, numDiophantineSolns it)
--    where it = p108 110880 4000000
-- Solved [Mon Nov 26 2007 / 03:27 EST]

--p110 [Mon Nov 26 2007 / 03:27 EST]
hcn = 43243200:(map (\last -> hcn' (length $ divisors last) last) hcn)
hcn' maxD start = head $ filter ((>maxD) . length . divisors) [start+1..]

hdn :: [Int]
hdn = 1:2:4:6:12:24:30:
      map (\last -> head $ let ndsl = numDiophantineSolns2 last in
                          filter (\x -> (upperBoundNDS x > ndsl && numDiophantineSolns2 x > ndsl))
                                     [last+10,last+20..])
      (drop 6 hdn)
hdnDisplay = map (\n -> (n,numDiophantineSolns2 n)) $ hdn

--main = mapM_ print hdn
--main = mapM_ print $ zip hcn (map (length.divisors) hcn)

smallestWithTau tau = product $ zipWith (^) primes
                      (reverse $ map (subtract 1) $ factor tau)
--bigCandidates :: [Int]
--p110 = map (\(a,b) -> trace (show (b,a)) $ numDiophantineSolns (product (take a primes) * product (take b primes))) thePairs
--main = mapM_ print p110

thePairs = drop 8 $ [(a,b) | a <- [9..14], b <- [1..a],
                         (product (take a primes) * product (take b primes)) < 614889782588491410]
numDiophantineSolns2 n = (tau (n^2) + 1) `div` 2

p110succ state = map (sort . (:state)) $
                 (2:) $ map (snd . snd) $
                 filter (\((prevLen,prevPrime),(curLen,curPrime)) -> curLen < prevLen) $
                 let pairs = zip ((++[0]) $ map length $ group state) primes
                 in zip pairs (tail pairs)
p110util state = product $ map ((+1) . (*2) . length) $ group $ sort $ state
p110end state = p110util state > 8000000
p110cost state = product state
p110 = astar [] p110succ p110end p110cost (\state -> 8000000 - p110util state)
main = print p110

--p61
polygonal r n = n*((r-2)*n-(r-4))`div`2
fourDigitRGonals r = dropWhile (<1000) $ takeWhile (<10000) $
                     (map (polygonal r) [1..])
digit d n = (iterate (`div`10) n) !! (4-d) `mod` 10
p61 = concatMap p61' (permutations [3..8])
p61' rs@[r1,r2,r3,r4,r5,r6] = do
  x <- fourDigitRGonals r1
  y <- filter (\y -> digit 1 y == digit 3 x &&
                      digit 2 y == digit 4 x) $ fourDigitRGonals r2
  z <- filter (\z -> digit 1 z == digit 3 y &&
                      digit 2 z == digit 4 y) $ fourDigitRGonals r3
  a <- filter (\a -> digit 1 a == digit 3 z &&
                      digit 2 a == digit 4 z) $ fourDigitRGonals r4
  b <- filter (\b -> digit 1 b == digit 3 a &&
                      digit 2 b == digit 4 a) $ fourDigitRGonals r5
  c <- filter (\c -> digit 1 c == digit 3 b &&
                      digit 2 c == digit 4 b &&
                      digit 3 c == digit 1 x &&
                      digit 4 c == digit 2 x &&
                      True
              ) $ fourDigitRGonals r6
  return $ x+y+z+a+b+c
-- p61 Solved [Wed Dec 12 2007 / 06:54 EST]
--p60 [Wed Dec 12 2007 / 07:40 EST]
intConcat x y = x * 10^(length (show y)) + y --fastest
--intConcat x y = read $ (show x) ++ (show y)
--intConcat x y = x * 10^(ceiling $ logBase 10 (fromIntegral y)) + y
primePartition m 1 n = if m < n && isPrime n then return [n] else fail "not prime"
primePartition m parts n = do
  p <- dropWhile (<m) $ takeWhile (<n) (tail primes)
  rest <- primePartition p (parts-1) (n - p)
  if all (isPrime . (`intConcat` p)) rest  &&
     all (isPrime . (p `intConcat`)) rest  &&
      True
    then return (p:rest)
    else fail "no partition"
p60 = head $ filter ((/=[]) . (primePartition 1 5) . traceIt) [2043,2045..]

--p68
fst3 (a,b,c) = a
checkRing l = checkRing' (l ++ [head l])
checkRing' (x:y:r) = ((14 - x - y), x, y) : checkRing' (y:r)
checkRing' _ = []
magicString = concatMap (\(a,b,c) -> show a ++ show b ++ show c)
p68' = filter ((==5).length.nub.(map fst3)) $
       filter (all ((>=6).fst3)) $
       filter (all ((<= 10).fst3)) $
       map checkRing $ permutations [1..5]
p68 = filter ((=='6').head) $ sort $ map magicString p68'
-- Solved [Mon Dec 24 2007 / 12:25 EST]      

-- p116
red   = fibs
green = 1:1:2:(zipWith (+) green (tail (tail green)))
blue  = 1:1:1:2:(zipWith (+) blue (tail (tail (tail blue))))
p116' n = red !! n - 1 + green !! (n-1) - 1 + blue !! (n-1) - 1
p116 = p116' 50
-- Solved [Mon Dec 24 2007 / 15:39 EST]

-- p117
p117' n = p117 !! (n-1) +
          p117 !! (n-2) +
          p117 !! (n-3) +
          p117 !! (n-4)
p117 = 0:1:2:4:8:(map p117' [5..])

--p120 [Mon Dec 24 2007 / 23:32 EST]
generator m n = take n $ iterate ((`mod`n).(*m)) m
rMax a = trace (show a) $ maximum $ map (`mod`(a^2)) $
         zipWith (+) (generator (a-1) (a^2)) (generator (a+1) (a^2))
p120 = sum $ map rMax [3..1000]
-- Solved [Tue Dec 25 2007 / 00:06 EST]

--p114
p114' 0 = 1
p114' 1 = 1
p114' 2 = 1
p114' n = sum (map p114 [n-4,n-5..0]) + 1 + p114 (n-1)
p114 n | n < 0 = 0 
       | otherwise = (p114cache !! n)
p114cache = map p114' [0..]

p115' n
    | n >= 50 = sum (map p115 [n-51,n-52..0]) + 1 + p115 (n-1)
    | otherwise = 1
p115 n | n < 0 = 0 
       | otherwise = (p115cache !! n)
p115cache = map p115' [0..]

--p127
memoize :: (Num a, Enum a) => (a -> b) -> a -> (a -> b)
memoize f minBound = (cache !!).fromEnum.(subtract minBound)
    where cache = map f [minBound..]

cRadRatio c = c `div` (rad c)
mc = memoize cRadRatio 0
possibleC = filter ((>=2).mc) [1..9999]
abchit (a,b,c) =
    gcd a b == 1 &&
    gcd a c == 1 &&
    gcd b c == 1 &&
    (rad (a*b)) < mc c
p127' = [(c-b,b,c) | c <-possibleC, b <- [c`div`2..c-1]]
p127 = --scanl1 (+) $
       map (\(a,b,c) -> c) $
       filter abchit p127'
--main = mapM_ print (zip p127 (scanl1 (+) p127))
--possibleBs primeBases
{-abchits c =
    let crr = cRadRatio c in
    let abRads = Combinatorics.choose 2 $ sortNub $ map rad $
                 map (242`div`) $ sortNub $ map rad $ divisors crr in
    -}