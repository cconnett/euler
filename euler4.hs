module Main where

import Euler
import NumberTheory
import Combinatorics
import Primes
import Factor
import Probability
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
import Data.Array.Unboxed
import Control.Parallel
import Control.Parallel.Strategies

divideNextState d state = (state * 10 + 1) `mod` d
divisionStates r d = iterate (divideNextState d) 1
repunitDivides r d
    | neverDivides r d = False
    | otherwise = --(if d `mod` 10000 < 50 then trace (show d) else id) $
                  (r-1) `mod` cycleLength r d == distanceToFirstZero r d
distanceToFirstZero r d = length $ takeWhile (/=0) $ divisionStates r d
cycleLength r d = 1 + (length $ takeWhile (/=1) $ drop 1 $ divisionStates r d)
neverDivides r d = not $ elem 0 (takeWhile (/=1) $ drop 1 $ divisionStates r d)
p132 = take 40 $ filter (repunitDivides (10^9)) primes
p132answer = [11,17,41,73,101,137,251,257,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,10753,15361,16001,19841,21001,21401,24001,25601,27961,37501,40961,43201,60101,62501,69857,76001,76801,160001]
--main = mapM_ print p132

--p158
numStringsFromTo a z len = nsdp ! (a, z, len)
nsdp :: Array (Char, Char, Int) [String]
nsdp = listArray (('a', 'a', 0), ('z', 'z', 26)) $
       [numStringsFromTo' a z len | a <- ['a'..'z'], z <- ['a'..'z'], len <- [0..26]]
numStringsFromTo' :: Char -> Char -> Int -> [String]
numStringsFromTo' a z 0 = [""]
numStringsFromTo' a z 1
    | a == z = [[a]]
    | otherwise = []
numStringsFromTo' a z 2
    | a == z = []
    | otherwise = [[a,z]]
numStringsFromTo' a z 3 = [[a,b,z] | b <- [succ a..pred z]]--max 0 $ ord z - ord a - 1
numStringsFromTo' a z len
    | a == z = []
    | otherwise = map (a:) $
                  concat [numStringsFromTo b z (len-1) | b <- [succ a..z]]

--sum [numStringsFromTo b z (len-1) | b <- [succ a..z]]
p158p n = concat
          [[(s1,s2) | s1 <- numStringsFromTo a y n', s2 <- numStringsFromTo x z (n-n')]
           --,(a,z,y,x,n'))
                   | a <- ['a'..'z'],
                     z <- ['a'..'z'],
                     y <- [a .. 'z'],
                     x <- ['a' .. pred y],
                     n' <- [1..n-1]]

simplex17 = [[a,b,c,d,e,f,g,h,i,j] | a <- [0..3], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], f <- [0..3], g <- [0..3], h <- [0..3], i <- [0..3], j <- [0..3], sum [a,b,c,d,e,f,g,h,i,j] == 17]
digits18 simplex =
    length freeDigits * (factorial 17 `div` (product $ map factorial simplex))
    where freeDigits = filter ((<3) . snd) $ zip [1..9] (tail simplex)
p172 = sum $ map digits18 simplex17

--p181 [Thu Jun 12 2008 / 12:29 EDT]
p181dp :: Array (Int, Int) Int
p181dp = listArray ((0,0),(60,40)) $
         [combos b w | b <- [0..60], w <- [0..40]]
combos 1 0 = 1
combos 0 1 = 1
combos b w = sum [p181dp ! ((b-bi),(w-wi))
                      | bi <- [0..b], wi <- [0..w], b+w > 0]

--
lagFibInit k = (100003 - 200003*k + 300007*k^3) `mod` 1000000
first55 = [lagFibInit k | k <- [1..55]]
lagFibList = (first55 ++) $ map (`mod`1000000) $
             zipWith (+) lagFibList (tails lagFibList !! 30)
--
--p111 [Thu Jun 12 2008 / 21:53 EDT]
--dDigitsMRepeated :: Int -> Int -> [Int]
dDigitsMRepeated rd d m = map intify $ filter ((/=0) . last) $
                          dDigitsMRepeated' rd d m
dDigitsMRepeated' :: Int -> Int -> Int -> [[Int]]
dDigitsMRepeated' rd d m
    | m == 0 = dDigitsWithoutX d rd
    | d == m = [replicate d rd]
    | otherwise = do
  nextDigit <- [0..9]
  let others = if nextDigit == rd then
                   dDigitsMRepeated' rd (d-1) (m-1) else
                   dDigitsMRepeated' rd (d-1) m
  nextNumber <- others
  --trace (show d) $ trace (show m) $
  return $ nextDigit:nextNumber

intify :: [Int] -> Int
intify [] = 0
intify (d:digits) = d + 10 * intify digits

dDigitsWithoutX 1 x = map (:[]) $ filter (/=x) [0..9]
dDigitsWithoutX d x = do
  thisDigit <- filter (/=x) [0..9]
  number <- dDigitsWithoutX (d-1) x
  return $ thisDigit:number

p111 = 612407567715

--p206
qr n = sortNub $ map ((`mod`n) . (^2)) $ [0..n`div`2]
--p174
countLaminae t4 = length $ filter (\x -> t4`div`x-x+1>=2) (divisors t4)
p174 = length $ filter (\t4 -> let l = countLaminae t4 in 1 <=l && l <= 10) [1..250000]
--p203
p203 = sum $ filter (\n -> length (factor n) == length (sortNub $ factor n)) $ sortNub $ concat $ take 51 pascalsTriangle
--p214
primeChainLengthEq p m = (iterate totient (p-1)) !! (m-2) == 1
p214 = sum $ filter (\p -> primeChainLengthEq p 25) $ takeWhile (<40000000) primes
--p204
--25 primes are <= 100
--hamming :: [Integer]
p204 = hamming
hamming = hamming' 1 0 1
hamming' curProd curPrimeNo curPrime
    | curPrimeNo == 25 = if curProd <= 1000000000 then 1 else 0
    | otherwise = sum $ takeWhile (/=0) $
                  let np = nextPrime curPrime in
                  map (\nextFactor -> hamming' (curProd*nextFactor)
                                               (curPrimeNo+1)
                                               np) (iterate (*np) 1)
--p131
check131 p = any (\n-> perfCube $ (n+p)*n*n) $ possibleN p
possibleN p = filter (>0) $ map (subtract p) $ possibleCubes1 p
possibleCubes1 p = takeWhile (<2*p) $ map (^3) [1..]

possibleCubes n = map (^3) [1..1000]
gen131 n =
    let residual =
            product $ -- (n+p) can now only be a cube multiple of this number
            map (\(p,k) -> p^((-k)`mod`3)) $ -- Figure out what factors need to be in the (n+p) term to complete the cube
            map (\(p,k) -> (p,2*k)) $ -- Square n
            filter (\(p,k) -> k`mod`3/=0) $ -- Remove cubed prime factors
            map (\g -> (head g, length g)) $ -- Get powers of each prime in n
            group $ factor n
    in filter isPrime $ map (\cube -> cube * residual - n) (possibleCubes n)
p131 = do
  let them = takeWhile (<1000000) $ concatMap gen131 [1..]
  mapM_ print them
  putStr "Answer: "
  print $ length them
{-
    let residual =
            product $ -- (n+p) can now only be a cube multiple of this number
            map (\(p,k) -> p^((-k)`mod`3)) $ -- Figure out what factors need to be in the (n+p) term to complete the cube
            map (\(p,k) -> (p,2*k)) $ -- Square n
            filter (\(p,k) -> k`mod`3/=0) $ -- Remove cubed prime factors
            map (\g -> (head g, length g)) $ -- Get powers of each prime in n
            group $ factor n
    in filter isPrime $ map (\cube -> cube * residual - n) (possibleCubes n)
-}
--p183
e = 2.7182818
bestRatios :: [(Rational,Integer)]
bestRatios = map (\n -> let k = round (fromIntegral n/e) in (n%k,n)) [5..10000]
terminates ratio = all (\f -> f==2 || f==5 || f==1) $ factor $ denominator ratio
p183 = sum $ map (\(ratio,n) -> if terminates ratio then -n else n) bestRatios

--p138
checkBase b = filter (/=(0,0,0)) $
              map (\h -> let lSquared = (h^2 + (b`div`2)^2)
                         in if perfSquare lSquared then
                                (b,h,round (sqrt $ fromInteger lSquared)) else
                                (0,0,0))
                      [b-1,b+1]
p138' = filter (\(b,h,l) -> h == (2*b)-1 || h == (2*b)+1 ) $
        primitivePythagoreanTriples2
{-main = do
  --let them = take 12 $ concatMap checkBase [4,8..]
  let them = take 1 p138'
  mapM_ print them
  putStr "Answer: "
  print $ sum $ map (\(b,h,l) -> l) them
-}
primitivePythagoreanTriples2 =
    [getTriple m n | m <- [472142416783479..], n <- [-2*m - integerRoot 2 (1+5*m*m), -2*m + integerRoot 2 (1+5*m*m)],
     n > 0, gcd m n == 1, odd m && even n || even m && odd n]
{-
(8,15,17)
(136,273,305)
(2448,4895,5473)
(43920,87841,98209)
(788120,1576239,1762289)
(14142232,28284465,31622993)
(253772064,507544127,567451585)
(4553754912,9107509825,10182505537)
(81713816360,163427632719,182717648081)
(1466294939560,2932589879121,3278735159921)
(26311595095728,52623190191455,58834515230497)
(472142416783536,944284833567073,1055742538989025)
Answer: 1118049290473932
... fuck yeah.
-}
triangleNumber n = n*(n+1)`div`2
fold148 [] = 0
fold148 ((p,d):rest) = triangleNumber d * 28^p + ((d+1)*fold148 rest)

--p211
p211 = filter (perfSquare . sum . map (^2) . divisors) $
       filter (not . isPrime) $ map (traceN 1000000) [1..64000000-1]
{-main = do
  mapM_ print p211
  print $ length p211
-}
--p225
tribs :: [Integer]
tribs = 1:1:1:zipWith3 (\a b c -> a+b+c) tribs (tail tribs) (tail (tail tribs))
--p135
{-
p135 = map fst $ map head $
       filter ((==10) . length) $
       groupBy (equating fst) $
       sort $ filter (\(n,(a,b)) -> 1 <= n && n < 1000000) $
       --filter ((==1000000) . fst) $
       --filter ((==(1969,394)) . snd) $
       concatMap solnGenerator [2..6000000]
    where f a b = (6*a*b - a*a - 5*b*b, (a, b))
          solnGenerator a = map (f a) $ [lower a - 2..upper a + 2]
lower a = floor $ (3*fromInteger a)/5 - 1/5*sqrt(-5+4*fromInteger (a*a))
upper a
    | a <= 1154 = a`div`2
    | a > 1154 = ceiling $ 3*fromInteger a/5 - 2/5*sqrt(fromInteger $ (a*a) - 1250000)
-}
p135d n d = do
  if not (4 `divides` ((n`div`d) + d)) then
      fail "bad divisor" else
      let b = (n`div`d + d) `div` 4 in
      let a = n`div`d + b in
      if a > 0 && b > 0 && a > 2*b then
          Just (a,b) else
          fail "invalid a or b"
p135s n = mapMaybe (p135d n) (divisors n)
p135f a b = 6*a*b - a*a - 5*b*b
p135 = length $ filter ((==10) . length) $ map p135s [1..1000000]

p136 = filter ((==1) . length . p135s) [1..50000000]
p136s n = take 2 . p135s
{-main = do
  mapM_ print p130
  putStrLn ""
  print $ sum p130
-}
p130a n = head $ filter (`repunitDivides`n) [1..]
p130 = take 25 $
       filter (\c -> (not $ 5`divides`c) &&
                     isComposite c &&
                     any (\d -> repunitDivides d c) (divisors (c-1))) [91,93..]
p129 = head $
       filter (\c -> (not $ 5`divides`c) &&
                     --isComposite c &&
                     p130a c > 10000)
              [1411,1413..]
--p231
p231 = sum (concatMap factor [15000001..20000000]) -
       sum (concatMap factor [2..5000000])
