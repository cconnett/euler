{-# OPTIONS -fglasgow-exts #-}

import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Primes
import Factor
import Debug.Trace
import EulerData
import Ratio
import IO
import Array
import Maybe
import Monad
import Control.Exception as E
import Data.Bits (xor)
import Data.Ord
import Control.Arrow

import Euler
import NumberTheory
import Combinatorics

--slower p = (\arg -> (any p (replicate 1000000 arg)) && all p (replicate 1000000 arg))

infixr 7 !!!
(!!!) as 0 = head as
(!!!) [a] n = a
(!!!) as n = (tail as) !!! (n-1)

-- p1
p1 = sum $ filter (\n -> any ((==0).(n`mod`)) [3,5])  [1..999]

-- p2
p2 = sum $ filter even $ takeWhile (<1000000) fibs

-- p4
tdn = [100..999]
p4 = maximum $ filter palindromic $ [a*b | a <- tdn, b <- tdn]

--p8
l = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
ss = [take 5 $ drop x l | x <- [0..995]]
p8 = maximum $ map (product.(map (read.(:[])))) ss

--p9
partitionInt = integerPartition

--p11
p11 = maximum $ map product (rows++cols++diags)
get4s l = takeWhile ((==4).length) $ [take 4 $ drop x l | x <- [0..]]
rows = concatMap get4s p11data
cols = concatMap get4s (transpose p11data)
diags = concatMap get4s (allDiags p11data)
allDiags :: [[Int]] -> [[Int]]
allDiags grid = [[(grid !! r) !! (r-offset) | r <- [0..19],
                                              0 <= r-offset && r-offset <= 19]
                     | offset <- [-19..19]] ++
                [[(grid !! r) !! (offset-r) | r <- [0..19],
                                              0 <= offset-r && offset-r <= 19]
                     | offset <- [0..38]]

--p12
triangs = 1:zipWith (+) [2..] triangs
--numDivisors n = product $ map (\ps -> (length ps + 1)) $ group $ factor n
p12 = head $ filter ((>500).numDivisors) triangs

--p14
f n
  | even n = n `div` 2
  | odd n = 3*n + 1

chain = (takeWhile (/=1)).(iterate f)
p14 = head $ maximumBy (comparing length) $ map chain [1..999999]

--p15
routes 1 1 = 2
routes 1 m = m
routes n 1 = n
routes n m = routes (n-1) m + routes n (m-1)
p15 = routes 20 20

--p17
toWords :: Int -> String
toWords 1 = "one"
toWords 2 = "two"
toWords 3 = "three"
toWords 4 = "four"
toWords 5 = "five"
toWords 6 = "six"
toWords 7 = "seven"
toWords 8 = "eight"
toWords 9 = "nine"
toWords 10 = "ten"
toWords 11 = "eleven"
toWords 12 = "twelve"
toWords 13 = "thirteen"
toWords 14 = "fourteen"
toWords 15 = "fifteen"
toWords 16 = "sixteen"
toWords 17 = "seventeen"
toWords 18 = "eighteen"
toWords 19 = "nineteen"
toWords 20 = "twenty"
toWords 30 = "thirty"
toWords 40 = "forty"
toWords 50 = "fifty"
toWords 60 = "sixty"
toWords 70 = "seventy"
toWords 80 = "eighty"
toWords 90 = "ninety"
toWords 1000 = "onethousand"
toWords n
    | n < 100 = toWords ((n`div`10) * 10) ++ toWords (n`mod`10)
    | n `mod` 100 == 0 = toWords (n`div`100) ++ "hundred"
    | otherwise = toWords (n`div`100) ++ "hundredand" ++ toWords (n`mod`100)

--p21
d n = (sum $ properDivisors n)

amicable n = d n /= n && d (d n) == n

--p22
readStrings :: String -> IO [String]
readStrings filename = do
  h <-openFile filename ReadMode
  contents <- hGetContents h
  return $ read $ "["++contents++"]"
alphaScore = sum.(map ((subtract 64).(ord)))
p22 = do
  names <- readStrings "names.txt"
  return $ sum $ zipWith (*) [1..] $ map alphaScore $ sort names

--p23
abundant n = d n > n

abuns = filter abundant [1..28124]
--abunSums = nub $ [a+b | a <- abuns, b <- abuns, a < b, a+b < 28124]
--p23 = S.difference (S.fromList [1..28124]) (S.fromList abunSums)
isAbunSum n = any (\a -> a `elem` abuns && (n-a) `elem` abuns) [1..(n+2)`div`2]
nonAbunSums = filter (not.isAbunSum) [1..28124]
p23 = sum nonAbunSums

--p26
approxCycleLength :: (Eq a) => [a] -> Int
approxCycleLength as = 1 + (minimum $ map approxCycleLength' (take 1 $ tails as))
approxCycleLength' :: (Eq a) => [a] -> Int
approxCycleLength' as = maybe 0 id $ elemIndex True $ map (flip apparentCycle as) [1..2000]
apparentCycle :: (Eq a) => Int -> [a] -> Bool
apparentCycle len as = ((length $ nub $ take 20 $ chunks len as) == 1) && ((length $ take 2 $ chunks len as) > 1)
chunks len as = (take len as):(chunks len (drop len as))
p25 = maximum $ map (\d -> trace ((show d) ++ " " ++ (show $ acl d)) (acl d, d)) range
    where range = [999,997..1]
          acl d = approxCycleLength (fractionalDigits (1%d))

--p27
coeffs = [(a, b) | a <- [-999..999], b <- [-999..999]]
quadratic (a, b) n = n^2 + a*n + b

p27 = maximum primeLengths
primeLengths = map (\coeff -> (primeLength $ quadratic coeff, coeff)) coeffs
primeLength f = length $ takeWhile isPrime (map f [0..])

--p28
p28 = sum $ fourDiags 1001

--p30
fifthPowerDigitsSum n = n == (sum $ map ((^5).read.(:[])) (show n))
p30 = filter fifthPowerDigitsSum [1..]

--p31
vals = reverse $ [1,2,5,10,20,50,100,200,500] :: [Int]
ways :: Int -> [Int] -> [Int]
ways tot [coin]
    | tot == 0 = [1]
    | tot `mod` coin  == 0 = [1]
    | otherwise = [0]
ways tot vals = do
  let coin = head vals
  num <- [0..tot`div`coin]
  return $ sum $ ways (tot - num*coin) (tail vals)

--p32

p32 = sum $ filter pandigitalProductIdentity [1..10000]
--powerset :: [a] -> [[a]]
--powerset = filterM (const [True, False])
pandigitalProductIdentity prod = any (factorSplitPandigital prod) (powerset (factor prod))
factorSplitPandigital prod factorSet =
    let a = product factorSet
        b = prod `div` a
    in [1..9] == (nub $ sort $ (digits a) ++ (digits b) ++ (digits prod))

--p33 [Sat Sep 15 2007 / 14:37 EDT]
fracPairs = [(n,d) | d <- [10..99], n <- [10..d-1]]
cancelled n d = filter (\(n,d) -> d/=0)
                (concat [--cancelBy div div mod mod n d,
                         cancelBy mod div div mod n d,
                         cancelBy div mod mod div n d,
                         cancelBy mod mod div div n d])

cancelBy c1 c2 c1o c2o n d =
    if n`c1o`10 == d`c2o`10
    then [(n`c1`10,d`c2`10)]
    else []
                
unorthFracs = filter (\(n,d) -> any (\(cn,cd) -> n%d == cn%cd) (cancelled n d)) fracPairs
p33 = product $ map (uncurry (%)) unorthFracs
-- Solved [Sat Sep 15 2007 / 15:01 EDT]
         
--p34
p34 = filter (\n -> n == (sum $ map factorial( digits n))) [3..]

--p35
circularPrime n = all isPrime (rotations n)
rotations n = map joinDigits $
              take (length (digits n)) (iterate rotate (digits n))
rotate (x:xs) = xs++[x]
p35 = length $ filter circularPrime (takeWhile (<1000000) (primes::[Int]))

--p36
palindromic10 n = (show n) == reverse (show n)
palindromic2  n = (show2 n) == reverse (show2 n)
show2 0 = ""
show2 n = show2 (n`div`2)++[if even n then '0' else '1']
p36 = sum $ filter (\n -> palindromic2 n && palindromic10 n) [1..999999]

--p37
--truncFromRight = joinDigits.init.digits
truncFromLeft = joinDigits.tail.digits
truncFromRight = (`div` 10)

truncAllFromRight n = takeWhile (>0) $ tail $ iterate truncFromRight n
truncAllFromLeft n = takeWhile (>0) $ tail $ iterate truncFromLeft n

p37 = sum $ take 11 $ drop 4 $ filter truncatable primes
truncatable p = all isPrime (truncAllFromLeft p ++ truncAllFromRight p)

--p38
makePandigital :: Int -> Maybe [Int]
makePandigital n = makePandigital' n 1 []
makePandigital' n m as
    | length as > 9 = Nothing
    | length as == 9 && (sort as == [1..9]) = Just as
    | length as == 9 = Nothing
    | otherwise = makePandigital' n (m+1) (as ++ (map (read.(:[])) (show (n*m))))

--p40
p40 = product $map (read.(:[]).(string!!)) [10^a-1 | a <- [0..6]]
    where string = concatMap show [1..]

--p41 [Sat Sep 15 2007 / 15:01 EDT]
nPandigital x = (sort $ show x) == take n ['1'..]
    where n = length $ show x
billion = 10^9
p41 = head $ filter nPandigital $ filter isPrime [billion,billion-1..1]
--main = putStrLn $ show p41
--Solved in C [Sun Sep 16 2007 / 02:18 EDT]

--p43
p43 :: Int
p43 = sum $ filter ssdp $ map (read.concat.(map show)) $ permutations [0..9]
ssdp n =
    n `div` 10^0 `mod` 10^3 `mod` 17 == 0 &&
    n `div` 10^1 `mod` 10^3 `mod` 13 == 0 &&
    n `div` 10^2 `mod` 10^3 `mod` 11 == 0 &&
    n `div` 10^3 `mod` 10^3 `mod` 7 == 0 &&
    n `div` 10^4 `mod` 10^3 `mod` 5 == 0 &&
    n `div` 10^5 `mod` 10^3 `mod` 3 == 0 &&
    n `div` 10^6 `mod` 10^3 `mod` 2 == 0

--p45
pentags :: [Int]
pentags = map (\n -> n*(3*n-1)`div`2) [1..]
hexags :: [Int]
hexags = map (\n -> n*(2*n-1)) [1..]
intersect3 (s1, s2, s3) = intersectSorted [s1, s2, s3]
intersectSorted = foldl1 intersectSorted2
intersectSorted2 s1 s2
    | head s1 == head s2 = head s1:intersectSorted2 (tail s1) (tail s2)
    | head s1 == (min (head s1)  (head s2)) = intersectSorted2 (tail s1) s2
    | head s2 == (min (head s1)  (head s2)) = intersectSorted2 s1 (tail s2)

--google treasure hunt prob 4
sums n = map (sum . (take n)) (tails primes)
gth4 = intersectSorted [sums 9, sums 25, sums 57, sums 827]
main = print $ head $ filter isPrime $ gth4
       
--p44 [Wed Sep 12 2007 / 02:06 EDT]
inds = [(a, t-a) | t <- [1..], a <- [1..t`div`2]]
pentag n = n*(3*n-1)`div`2
isPentag p = p == (head $ filter (>=p) pentags)
p44 = let (x,y) = break (\(a,b) -> (isPentag (pentag b - pentag a) && 
                                    isPentag (pentag b + pentag a))) inds
      in head y
-- Solved [Wed Sep 12 2007 / 17:21 EDT] by using optimized code and trust

--p46 [Wed Sep 12 2007 / 17:23 EDT]
oddComposites = filter (not.isPrime) [9,11..]
goldbachsPredicate oc = True
p46 = head $ filter (not.goldbachsPredicate) oddComposites


--p47 [Wed Sep 12 2007 / 17:40 EDT]
p47 = head $ filter (\n -> all (\m -> 4 == (length $ nub $ factor m)) [n..n+3]) [647..]
--Solved [Wed Sep 12 2007 / 17:43 EDT]
       
--p52
p52 = filter multPermute [1..]
    where multPermute x = 1 == (length $ nub $ map (sort.show) (map (*x) [1..6]))

--p55
lychrel n = not $ any palindromic $ take 50 $ tail $ iterate reverseAdd n
reverseAdd n = n + (joinDigits.reverse.digits) n
p55 = length $ filter lychrel [1..9999]
                          
--p56
p56 = maximum $ map digitalSum [a^b | a <- [2..100], b <- [2..100]]

--p57
sqrt2 depth = (sqrt2's !! depth) - 1

sqrt2's :: [Ratio Integer]
sqrt2's = map sqrt2' [0..1000]

sqrt2' 0 = 2
sqrt2' depth = 2 + (1/(sqrt2's !! (depth-1)))

p57 = length $ filter biggerNumerator $ map sqrt2 [1..1000]
    where biggerNumerator r = (length $ show $ numerator r) > (length $ show $ denominator r)

--p58
partialSums seed as = seed:(partialSums (seed + head as) (tail as))
diagVals = partialSums 1 (concat (map (replicate 4) [2,4..]))
fourDiags s = take (4*s-3) diagVals
ratioPred p as = (fromIntegral $ length (filter p as)) / (fromIntegral $ length as)

p58 = findFirst ((<0.10).traceIt.(ratioPred isPrime).fourDiags) [7..]

--p59
p59 = do
  dat <- readFile "cipher1.txt"
  let ct = read ("["++dat++"]") :: [Int]
      keys = [[a,b,c] | a <- [245,244..22], b <- [0..255], c <- [0..255]]
  mapM_ (printADecryption ct) keys

printADecryption ct key = if all isAscii pt then putStrLn ((show key) ++ pt) else return ()
    where pt = (decryption (take 50 ct) key)
decryption ct key = map chr $ zipWith xor ct (concat $ repeat key)
  
       
--p63
p63 = [b^x | b <- [1..9], x <- [1..50], x == (length $ show $ b^x)]

--p65
continuedFraction :: Integral k => k -> [k] -> k -> Ratio k
continuedFraction a bseq depth = a%1 + continuedFraction' bseq depth
continuedFraction' [] _ = 0 % 1
continuedFraction' _  0 = 0 % 1
continuedFraction' bseq depth = 1 / (head bseq % 1 +
                                     continuedFraction' (tail bseq) (depth - 1))
eNotation = concat [[1, 2*k, 1] | k <- [1..]]
approxE = map (continuedFraction 2 eNotation) [0..10]
p65 = sum $ map (read.(:[])) $
      show $ numerator $ continuedFraction 2 eNotation 99
-- Solved [Fri Oct 19 2007 / 16:09 EDT]
           
--p69
p69 = head $ sort $ map (\n -> ((fromIntegral n)/(fromIntegral $ totient n), n)) $ [2..1000000]

p70 = head $ sort $ map (\n -> ((fromIntegral n)/(fromIntegral $ totient n), n)) $
      filter (\n -> (sort (digits n)) == (sort $ digits $ totient n)) [2..9999999]

--p72
p72 :: Integer
p72 = sum $ map totient [2..1000000]
       
--p73
rpfs md = [n % d | d <- [1..md],
                   n <- filter (\n -> gcd d n == 1) [d`div`3..d`div`2]]
p73 = length $ filter (\f -> 1%3 < f && f < 1%2) (rpfs 10000)

--p74
factorialize n = sum $ map factorial (digits n)
chainLength n = length $
                takeWhile (\chain -> (length $ nub chain) == (length chain)) $
                tail $ inits $ iterate factorialize n
p74 = length $ filter ((==60).chainLength) [1..999999]


--p76 & p78
gpcCache = map (\m -> map (gpc' m) [0..]) [0..]
gpc m b = gpcCache !! m !! b
gpc' m b =
    if m == 0 then 1 else sum $ map (\i -> gpc (m - i) i) [1..min m b]

p :: Int -> Integer
p m = gpc m m
ps = map p [0..]

p76 = p 100
p78 = length $ fst $ span ((/=0)) $ numPartitions
--p78start = 1 -- haven't found it below this
--p78 = takeWhile ((/=0).(`mod`1000000).fst) $ map (first countPartitions) $ zip [p78start..] [p78start..]
--main = print p78

--p77
gpcCache2 = map (\m -> map (gpc'2 m) [0..]) [0..]
gpc2 m b = gpcCache2 !! m !! b
gpc'2 m b =
    if m == 0 then 1 else sum $ map (\i -> gpc2 (m - i) i) (takeWhile (<=min m b) primes)

pPrimes :: Int -> Integer
pPrimes m = gpc2 m m
ps2 = map pPrimes [0..]

p77 = length $ fst $ break (>5000) ps2
       
--p84
data Square = Go | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL |
              C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP |
              E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J |
              G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
                   deriving (Show, Eq, Ord, Ix, Bounded, Enum)

infixr 6 !+
(!+) :: forall a. (Bounded a, Enum a) => a -> Int -> a
(!+) enum n = toEnum $
              ((fromEnum enum) + n) `mod` (fromEnum (maxBound :: a)-
                                           fromEnum (minBound :: a))
(!-) ix n = ix !+ (-n)

transitionProbs :: [Array Square Double]
--zeroes :: Array Square Double
--zeroes = array (GO, H2) (repeat 0)
transitionProbs = map transRow [Go .. H2]
transRow CC1 = listArray (Go, H2) $ fixed (1/16) Go $ fixed (1/16) JAIL $
               (map (*(14/16)) (dice CC1))
transRow CC2 = listArray (Go, H2) $ fixed (1/16) Go $ fixed (1/16) JAIL $
               (map (*(14/16)) (dice CC2))
transRow CC3 = listArray (Go, H2) $ fixed (1/16) Go $ fixed (1/16) JAIL $
               (map (*(14/16)) (dice CC3))
transRow CH1 = listArray (Go, H2) $
               fixed (1/16) Go $ fixed (1/16) JAIL $
               fixed (1/16) C1 $ fixed (1/16) E3 $
               fixed (1/16) H2 $ fixed (1/16) R1 $
               fixed (2/16) R2 $ fixed (1/16) U1 $
               fixed (1/16) T1 $
               (map (*(6/16)) (dice CH1))
transRow CH2 = listArray (Go, H2) $
               fixed (1/16) Go $ fixed (1/16) JAIL $
               fixed (1/16) C1 $ fixed (1/16) E3 $
               fixed (1/16) H2 $ fixed (1/16) R1 $
               fixed (2/16) R3 $ fixed (1/16) U2 $
               fixed (1/16) D3 $ 
               (map (*(6/16)) (dice CH2))
transRow CH3 = listArray (Go, H2) $
               fixed (1/16) Go $ fixed (1/16) JAIL $
               fixed (1/16) C1 $ fixed (1/16) E3 $
               fixed (1/16) H2 $ fixed (3/16) R1 $
                                 fixed (1/16) U1 $
               fixed (1/16) CC3 $
               (map (*(6/16)) (dice CH3))

fixed p sq rest = take (pos - 1) rest ++ [rest !! pos + p] ++ drop pos rest
    where pos = (fromEnum sq)
dice sq = rotateRight (fromEnum sq) $
       (map (/16) ([1..4] ++ [3,2,1] ++ replicate 33 0))
rotateRight n as = drop l as ++ take l as
    where l = length as - n

--p85
countR r c = (triangs !! (r-1)) * (triangs !! (c-1))
p85 = (\(a,b)->a*b) $ head $ sortBy (comparing err) [(a,b) | a <- [1..100], b <- [1..100]]
err (a, b) = abs (2000000 - (countR a b))

--p86
cuboidSolns m = sum [ab `div` 2 - (min (ab`div`2) $ max 0 (ab-c-1))
                         | ab <- [2..2*m], c <- [1..m], perfSquare (ab^2 + c^2)]
p86 = findFirst ((>1000000).cuboidSolns) [1..]

--p90
squares2d = map (^2) [1..9]
cubesFaces = choose 6 [0..9]
--p90 = filter canMakeSquares2d $
--      [(first, second) | first <- cubesFaces, second <- cubesFaces, first < second]
--canMakeSquares2d (first, second) =

--p91
points limit = [((x1, y1), (x2, y2)) | x1 <- [0..limit], y1 <- [0..limit],
                                       x2 <- [0..limit], y2 <- [0..limit],
                                      (x1, y1) /= (0,0), (x2, y2) /= (0,0),
                                      (x1, y1) /= (x2, y2),
                                      (x1 /= 0 || x2/=0), x1 /= x2 || (y1%x1 /= y2%x2)]
p91 = (flip div) 2 $ length $ filter hasRightAngle (points 50)
hasRightAngle ((x1, y1), (x2, y2)) =
    x1*x2+y1*y2 == 0 ||
      let (x1', y1') = (-x1, -y1)
          (x2', y2') = (x2-x1, y2-y1) in
      x1'*x2'+y1'*y2' == 0 ||
      let (x1', y1') = (-x2, -y2)
          (x2', y2') = (x1-x2, y1-y2) in
      x1'*x2'+y1'*y2' == 0

--p94
triangleArea' :: Integer -> Integer -> Ratio Integer
triangleArea' a b = (s*(s-a%1)*(s-a%1)*(s-b%1))
    where s = (a+a+b)%2
p94triangles = concat [[(a, a+1), (a, a-1)] | a <- [1..3333334]]
p94aet = filter (\(a,b) -> let area = triangleArea' a b in
                           denominator area == 1 &&
                            (perfSquare (numerator area)) &&
                            numerator area /= 0)
          p94triangles
p94fin = map (\(a,b) -> a+a+b) p94aet
--p94 :: Int
p94 = sum p94fin

sep n [] = []
sep n l
    | length l > n = (take n l) : (sep n (drop n l))
    | otherwise = [l]

--p95 [Sat Sep 15 2007 / 04:09 EDT]
amicableChain n = if (d $ last finiteChain) /= head finiteChain
                  then []
                  else finiteChain
    where finiteChain = iterate' [] d n

iterate' prevs f x
    | x > 1000000 || x `elem` prevs = []
    | otherwise = x : (iterate' (x:prevs) f (f x))

p95 = (minimum theChain, theChain)
    where theChain = amicableChain $
                     maximumBy (comparing (length.amicableChain)) [100000..200000]
-- Solved [Sat Sep 15 2007 / 14:36 EDT]
                  
--p99
baseExpPairs :: IO [(Integer, Integer)]
baseExpPairs = do
  dat <- readFile "base_exp.txt"
  return $ map read $ map (("("++).(++")")) $ lines dat

p99 = do
  g <- baseExpPairs
  return $ 1 + (fromJust $ (`elemIndex` g) $ maximumBy (comparing theLog) g)

theLog :: (Integer, Integer) -> Double
theLog (a, b) = (fromInteger b) * log (fromInteger a)

--p104
pd19 :: Integer -> Bool
pd19 = (=="123456789") . sort . show
top9 n = iterate (`div`10) n !! ((length $ show n) - 9)
topPd n = pd19 $ top9 n
botPd n = pd19 $ n `mod` 10^9
p104 = map fst $ filter (\(i, fibi) -> topPd fibi) $ drop 80 $ zip [1..] fibs

--p112
bouncy n = not (nonIncreasing x) && not (nonDecreasing x)
    where x = digits n

nonIncreasing (a:b:ns) = (a >= b) && nonIncreasing (b:ns)
nonIncreasing ([b]) = True
nonDecreasing (a:b:ns) = (a <= b) && nonDecreasing (b:ns)
nonDecreasing ([b]) = True

ratioHit pred ratio ns = ratioHit' pred ratio ns 0 0
ratioHit' pred ratio ns sat unsat =
    if pred (head ns)
    then if ((sat+1)%(sat+unsat+1) == ratio) then (head ns)
         else ratioHit' pred ratio (tail ns) (sat+1) unsat
    else ratioHit' pred ratio (tail ns) sat (unsat+1)
p112 = ratioHit bouncy (99%100) [21780..]


--p113
decrSeqs :: Int -> Int -> Int
decrSeqs n t = decrSeqsCache !! n !! t
decrSeqsCache = map (\n -> map (\t -> decrSeqs' n t) [0..]) [0..]
--decrSeqs' 1 1 = 1
--decrSeqs' 1 t 1 = 0
decrSeqs' 1 t = t
decrSeqs' n t
    | n <= 0 || t <= 0 = 0
    | otherwise = sum (map (decrSeqs (n-1)) [t,t-1..1])
p113 e = decrSeqs e 10 +
         (sum $ map ((subtract 1).((flip decrSeqs) 10)) [e,e-1..1]) + 1
         - e * 9 - 2

incr x = show x == (sort $ show x)
decr x = show x == (reverse $ sort $ show x)

--p123
p123 :: (Integer, Integer)
p123 = head $ filter ((>10^10).snd) $ map (\(n, pn) -> (n, (((pn-1)^n + (pn+1)^n)::Integer)`mod`(pn^2))) $ zip [1..] primes

--p124

p124 = snd $ (sort $ map (\n -> (rad n, n)) [1..100000]) !! 9999

--p131
--p131 = intersect3 [1..] primes (map (\n -> n^3 + ))

--p101
op :: [Integer] -> Int -> [Integer]
op u 1 = repeat (head u)
op u k = scanl (+) (head u) $ op (zipWith (-) (tail u) u) (k-1)
u n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
p101 = map (\s -> fst.head $ dropWhile (\(sn,un)->sn-un==0) $ zip s (map u [1..])) $
       map (op (map u [1..])) [1..10]
