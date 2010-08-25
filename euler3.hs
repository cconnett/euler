{-# LANGUAGE NoMonomorphismRestriction #-}
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
import qualified Data.Map as M
import qualified Data.Set as S
import AStar
import Control.Monad
import qualified Control.Exception as E
import Maybe
import Data.Array.IArray
import Data.Array.Unboxed
import Text.Printf
import Test.QuickCheck ((==>))
import Foreign

--p64
-- hacked up version of continuedFractionCoefficients from Euler.hs
-- do not use this function elsewhere
cfc :: [(Integer,Integer,Integer)] -> (Integer, Integer, Integer) -> Int
cfc visited arg@(n, addend, divisor)
    | arg `elem` visited = length visited - 1
    | otherwise =
        cfc (arg:visited) (n, -addend', (n - addend'^2)`div`divisor)
    where a_i = floor $ ((sqrt $ fromIntegral n) + (fromIntegral addend)) / (fromIntegral divisor)
          addend' = addend - a_i*divisor

p64 = length $ filter odd $
      map (\n -> trace (show n) $ cfc [] (n,0,1)) $
      filter (\n -> not $ (n`elem`(map (^2) [1..1000]))) [1..10000]

--p66
qdsMinX :: Integer -> Integer
qdsMinX d = trace (show d) $
            numerator $ head $
            filter (\r -> let x = numerator r
                              y = denominator r
                          in x^2 - d * y^2 == 1) $
            rationalConvergents d
nonSquares = filter (not . (`elem` (map (^2) [1..100]))) [1..1000]
p66 = reverse $! sort $! map (\d -> (qdsMinX d, d)) nonSquares
--main = print p66

--p80
p80 = sum $ map (\n -> sum $ traceIt $ take 100 $
                       fractionalDigits (rationalConvergents n !! 500)) $
      takeWhile (<=100) nonSquares

--p88
minimalPS1 k = head $ filter (\n -> any (==n) $ map product $
                                    integerPartition2 k n) [k+1..]
ksFor n =
    map (\factoring -> length factoring + (n - sum factoring)) $
    init $ improperFactorings n
minimalPS ((n,ks):more) seenKs goalSet
    | S.null $ goalSet `S.difference` seenKs = S.empty
    | otherwise =
        if (not . S.null) $ ((S.fromList ks) `S.difference` seenKs) `S.intersection` goalSet then
            S.insert n (minimalPS more (S.union seenKs (S.fromList ks)) goalSet) else
            minimalPS more seenKs goalSet
p88 = sum $ S.toList $
      minimalPS (zip nonPrimes $ map ksFor nonPrimes) S.empty (S.fromAscList [2..12000])
main = print p88
--p75 [Sat Jan 19 2008 / 19:19 EST]
--Perimeter of primitive pythagorean triples
pppt m n = 2*m^2 + 2*m*n
primPerims = [pppt m n | m <- [1..707], n <- [1..m],
              gcd m n == 1, odd m && even n || even m && odd n]
primPerimMultiples = concatMap (multiplesWhile (<=1000000)) primPerims
    where multiplesWhile p n = takeWhile p $ map (*n) [1..]
p75 = map head $ filter ((==1) . length) $ group $ sort primPerimMultiples

--p88 [Sun Jan 20 2008 / 03:30 EST]
psn k n = psn' k n n
psna = let k = 100
           n = 100 in
       listArray ((0,0,0),(k, n, n))
       [psn' k p s | k <- [0..k], p <- [0..n], s <- [0..n]]
           :: Array (Int,Int,Int) Bool
psn' k p s
    | k == 1 && p /= s = False
    | s == p + k - 1 = True
    | s > p = False
    | otherwise = any (\d -> psna ! (k-1, p`div`d, s-d))
                  (filter (<=s) $ reverse $ divisors p)
mpsn k = head $ filter (\n -> psn k n) [1..]

burns n
    | n == 1 || isPrime n = []
    | otherwise = do
  d <- tail $ divisors n
  let burn = n - n`div`d - d + 1
  return $ burn : concat (burns (n`div`d) ++ burns d)

--p98
anagrams a b = sort a == sort b
anagramGroups words = groupBy (equating sort) $ sortBy (comparing sort) words
squareAnagrams maxDigits =
    filter ((>1) . length) $
    anagramGroups $ map show $
    takeWhile ((<=maxDigits) . length . show) $
    map (^2) [1..]
possibleSquareAnagrams words maxDigits =
    anagramGroups $
    filter (\numString -> any (==map snd (tally numString)) (map (map snd . tally) words)) $
    concat $ squareAnagrams maxDigits
getWords :: [String]
getWords = unsafePerformIO $ do
  input <- readFile "words.txt"
  return $ read ("[" ++ input ++ "]")
--p98 = getWords >>= p98'
cryptogramMapping word = M.fromList $ zip (nub word) [1..]
cryptogramDirectMapping a b = M.fromList $ zip (nub a) (nub b)
applySubstitution s a = map (\k -> M.findWithDefault '-' k s) a
flop (a,b) = (b,a)
flipSubstitution = M.fromList . map flop . M.toList
--groupWorks :: [String] -> Int -> Bool
groupWorks aGroup aSquare =
    let it = map (\otherWord ->
                  let alist = zip (head aGroup) (show aSquare) in
                  let sub = cryptogramDirectMapping (head aGroup) (show aSquare) in
                  let str = applySubstitution sub otherWord in
                  if '-' `elem` str || head str == '0' || (length $ nub $ map fst $ nub alist) < (length $ nub alist)
                   then 0 else read str)
             (tail aGroup)
    in if all perfSquare it then maximum (filter perfSquare it) else 0
squaresOfLength n = map (^2) [lb..ub]
    where lb = ceiling $ sqrt (10^(n-1))
          ub = floor $ sqrt (10^n - 1)
testGroup aGroup = maximum $ map (groupWorks aGroup) (squaresOfLength (length $ head aGroup))
{-
p98' words = map (\group ->
                      let bigS = biggestSquare $ head group in
                      map (\word ->
                               let mapping = cryptogramDirectMapping word (show bigS) in
                               map (map (mapping M.!))
                 ) $
             interestingGroups words
-}

--p100
sqrt2 = sqrt 2
--p100 = map (\b -> (b+1, (ceiling $ sqrt2 * (geomMean b (b+1))))) $
--       filter checkOnIt [5*10^11..]
checkOnIt m = (if m `mod` 100000 == 0 then trace (show m) else id) $
              2*(gen m) == gen (floor $ sqrt2 * (geomMean m (m+1)))
gen m = m*m + m
--geomMean x y = sqrt $ fromIntegral (x*y)
geomMean x y = (fromIntegral $ x+y)/2

--p100 attempt 2
p100 :: [Integer]
p100 = --map (\b -> let d = ceiling $ sqrt $ fromIntegral $ 2*(b*b-b) in
       --           d) $
       filter (\b -> let d = ceiling $ sqrt $ fromIntegral $ 2*(b*b-b) in
                     d*d-d == 2*(b*b-b)) [756854806566..]
--main = print $ head p100

--p119
potentialA = nextA allIntegerPowerSeqs
allIntegerPowerSeqs = [map (\base -> (base^power, base, power)) [2..]
                                  | power <- [2..]]
nextA seqs = (head $ head seqs) :
             nextA (resorted ((tail (head seqs)) : (tail seqs)))
resorted seqs = insert (head seqs) (tail seqs)
insert seq seqs = let (front, back) = span ((<head seq) . head) seqs
                  in front ++ [seq] ++ back
p119 = take 30 $ zip [1..] $
       filter (\(bp,b,p) -> bp > 10) $
       filter (\(bp,b,p) -> bp == (digitalSum bp)^p) potentialA
--main = mapM_ print p119
-- Solved [Sat Dec 29 2007 / 02:29 EST]

--p122
efficientExponentiation = (cache!!)
    where cache = map effExp [0..]
effExp 0 = 0
effExp 1 = 0
effExp k = minimum $ map (\multis -> sum (sortNub multis) + length multis - 1) $
           map (map efficientExponentiation) $ init $ integerPartition k


--p125 [Sat Dec 29 2007 / 02:41 EST]
p125 = sum $ sortNub $ concat $ map (\n -> filter palindromic $ takeWhile (<10^8) $ tail $ scanl1 (+) $ map (^2) [n..]) [1..floor $ sqrt 10^8]
-- Solved [Thu Jan 24 2008 / 18:45 EST]

--p60
_N = 5

type State = ([Int], [Int])
successors (primeSet, remainingPrimes) =
    (primeSet, tail remainingPrimes) :
    if compatible primeSet (head remainingPrimes) then
        [(head remainingPrimes : primeSet, tail remainingPrimes)] else []
compatible primeSet newPrime =
    let newPrimeDigits = digits newPrime
        primeSetDigits = map digits primeSet
    in all (\oldPrimeDigits
            -> isPrime (joinDigits $ newPrimeDigits ++ oldPrimeDigits)) primeSetDigits
       &&
       all (\oldPrimeDigits
            -> isPrime (joinDigits $ oldPrimeDigits ++ newPrimeDigits)) primeSetDigits

goal (primeSet, remainingPrimes) =
    length primeSet >= _N
g = sum . fst
h (primeSet, remainingPrimes) = sum $ take (_N - length primeSet) remainingPrimes

p60 :: [([Integer], [Integer])]
p60 = astar ([], (tail primes)) successors goal g h
--main = do
  --print $ reverse $ fst $ head p60'
  --print $ sum $ fst $ head p60'
  --print p60'

p60' :: [(Int,Int,Int,Int,Int)]
p60' = [(a, b, c, d, e) | a <- takeWhile (<10000) primes,
              b <- takeWhile (<a) primes, compatible [a] b,
              c <- takeWhile (<b) primes, compatible [a,b] c,
              d <- takeWhile (<c) primes, compatible [a,b,c] d,
              e <- takeWhile (<d) primes, compatible [a,b,c,d] e]

--p90
arrangements = [(nineToSix a, nineToSix b) | a <- choose 6 [0..9],
                                             b <- choose 6 [0..9], a < b]
nineToSix = map (\x -> if x == 9 then 6 else x)
p90 = filter canMakeSquares arrangements
canMakeSquares (a, b) = all (\ [ai,bi] -> (ai `elem` a && bi `elem` b) ||
                                          (ai `elem` b && bi `elem` a))
                        (map (\ds -> if length ds == 1 then 0:ds else ds) $
                         map nineToSix $
                         map digits $
                         map (^2) [1..9])

--p93
infixr 6 ///
(///) :: Maybe (Ratio Int) -> Maybe (Ratio Int) -> Maybe (Ratio Int)
a /// b = do
  a' <- a
  b' <- b
  --trace (show a') $
   --trace (show b') $
  if b' /= 0 then
       return (a' / b') else
       fail "bad div"

exprs digits = do
  op1 <- ops
  op2 <- ops
  op3 <- ops
  permutations (op1 : op2 : op3 : map (Left . Just . (%1)) digits)
    where ops = [Right (liftM2 (+)),
                 Right (liftM2 (-)),
                 Right (liftM2 (*)),
                 Right (///)]

evaluate' ((a : b : stack), (Right mf : stream)) =
    evaluate' ((mf a b : stack), stream)
evaluate' (stack, Left c : stream) =
    evaluate' (c : stack, stream)
evaluate' ([Just r], []) = Just r
evaluate' _ = Nothing
evaluate stream = evaluate' ([], stream)

p93 = reverse $ sort $
      map (\digits -> (length1toN (S.fromList $ catMaybes $
                                   map evaluate $ exprs digits), digits)) $
      choose 4 [1..9]

length1toN set = length1toN' 1 set
length1toN' next set = if (next%1) `S.member` set then
                           1 + length1toN' (next+1) set else 0
--main = mapM_ print p93

--p109
vals = zip (repeat 'S') ([1..20] ++ [25])
doubles = zip (repeat 'D') $ map ((*2) . snd) vals
trebles = zip (repeat 'T') $ map (*3) [1..20]
score checkout@(firstTwo, third) =
    (sum $ map snd $ firstTwo) + snd third
checkouts' = do
  first <- vals ++ doubles ++ trebles
  second <- vals ++ doubles ++ trebles
  third <- doubles
  [(sort [first,second], third)] ++ [([second], third)] ++ [([], third)]
checkouts = sortNub checkouts'
p109 = length $ filter ((<100) . score) checkouts

--p122
succ122 values = map (:values) $ sortNub $ [a+b | a <- values, b <- values,
                                                       not (a+b `elem` values)]
heur122 k values = let diff = (k - maximum (filter (<k) values))
                   in case diff of
                        diff | diff `elem` values -> 1
                        diff | 2*diff `elem` values -> 2
                        diff | otherwise -> 3
minMult' k = astar [1] succ122 (k `elem`) length (const 0)
minMult = (subtract 1) . length . head . minMult'

start122 = 71
p122 = map minMult [start122..200]
--main = do
--  mapM_ print $ zip [start122..] p122
--  print $ sum p122
prop_everySum n = let final = (head $ minMult' n)
                  in all (`elem` [a+b | a <- final, b <- final]) (delete 1 final)

--p145 [Thu Jan 24 2008 / 15:04 EST]
p145 = length $ filter (\n -> all odd $ digits $ n + (joinDigits $ reverse $ digits n)) $
       map (\n -> if n`mod`1000000 == 0 then trace (show n) n else n) $
       filter ((/=0).(`mod`10)) [1..10^9]
--main = print p145

--p151

p151step configurations = do
  (ssEvents, envelopeContents) <- configurations
  Probability $ map (\paperSize ->
      (let newContents = sort ([paperSize+1..5] ++
                               delete paperSize envelopeContents) in
       (ssEvents + if length newContents == 1 then 1 else 0, newContents),
       1 % genericLength envelopeContents)) envelopeContents
p151' = take 17 $ iterate (reduceProbability . p151step) (Probability [((0, [1]), 1%1)])
-- look at probs of only days that could possibly have single sheets
--them = map (filter ((==1) . length . snd . fst)) $
--       map fromProbability $ take 17 $ p151'
--andthen = sum $ map (\l -> sum $ map snd l) (tail $ init them)
--p151 = take 8 $ rationalDigitsString $ (andthen) / 16
p151 = take 8 $ rationalDigitsString $ 
       sum $ map (\((ssEvents, emptyEnvelope), p) -> (ssEvents-1) * p) $
       fromProbability $ last p151'

--p151step configurations = 
--    concatMap (\(p, config) ->
--                   [ |

--p162
data HexNum = Num [Char] | Accept | Reject
              deriving (Eq, Ord, Show)
goodHexNum seq = '0' `elem` seq && '1' `elem` seq && 'A' `elem` seq
p162step (Num seq)
    | length seq == 16 = [(if goodHexNum seq then Accept else Reject, 1%1)]
    | otherwise = [(if goodHexNum seq then Accept else Num (nextDigit:seq),
                    1%16)
                       | nextDigit <- ['1'..'9'] ++ ['A'..'F']] ++
                  [(if goodHexNum seq then Accept else Reject, 1%16)]
p162step Accept = [(Accept, 1)]
p162step Reject = [(Reject, 1)]

p162' hexnums = do
  hexnum <- hexnums
  Probability $ p162step hexnum

--p164
ns9arr :: Array (Integer,Integer) Integer
ns9arr = listArray ((0,0), (99,20)) $ map (uncurry ns9) (range ((0,0),(99,20)))
--ns9 prevTwoDigits digitsRemaining = arr ! (prevTwoDigits, digitsRemaining)
ns9 prevTwoDigits 1 = 10 - digitalSum prevTwoDigits
ns9 prevTwoDigits digitsRemaining =
    sum $ map (\prevTwoDigits' -> ns9arr ! (prevTwoDigits',(digitsRemaining-1))) $
          map (\d -> prevTwoDigits `mod` 10 * 10 + d) $
          filter (\d -> digitalSum prevTwoDigits + d <= 9) [0..9]
p164 = ns9arr ! (00, 20) - ns9arr ! (00,19)

--p172 :: [Integer]
p172 = map calc $
       concatMap (\li -> intersperseAll li (replicate (10-length li) 0)) $
       filter (all (<=3)) $ filter ((<=10) . length) $ integerPartition 18
calc :: [Integer] -> Integer
calc ns = (17 `nCr` (ns !! 0)) * calc' ns
calc' ns = product $ zipWith (\ps n -> nCr (18-ps) n) partialSums (tail ns)
    where partialSums = scanl1 (+) ns

sqlam 1 _ = 0
sqlam 2 _ = 0
sqlam 3 x = if x >= 8 then 1 else 0
sqlam 4 x = if x >= 12 then 1 else 0
sqlam n x = let outerSize = 2*n + 2*(n-2) in
            if x >= outerSize then 1 + sqlam (n-2) (x-outerSize) else 0
p173 = sum $ takeWhile (>0) $ map (`sqlam` (10^6)) [3..]

p179 = sum $
       map ((subtract 1) . length) $
       filter ((>=2) . length) $
       group $ map (length . divisors) [2..10^7]
--main = print p179

--p134
capS p1 p2 = (p1 * (multInverse m p2)) `mod` m * p2
    where m = 10^(numDigits p1)
          numDigits = length . show
p134 = sum $ zipWith capS thesePrimes (drop 3 primes)
    where thesePrimes = takeWhile (<=1000000) $ drop 2 primes

--p152
--makeSum :: Integer -> [Integer] -> [[Integer]]
makeSum target [] _ _ = []
makeSum target inputs min max
    | target == 0 = [[]]
    | target == max = [inputs]
    | target < min = []
    | target > max = []
    | otherwise = let (i:rest) = inputs in
                  map (i:) (makeSum (target - i) rest min (max-i)) ++
                  makeSum target rest min (max-i)

top = 45
d152 = foldl lcm 1 $ map (^2) [2..top]
n152 = d152 `div` 2
s'152 = [d152 `div` x | x <- map (^2) [2..top]]
s152 = (d152 - sum s'152) : s'152
un152 n = floor $ sqrt $ fromIntegral $ denominator $ n % d152
p152 = map (map un152) $ makeSum n152 normals (last s'152) (sum s'152)
weirdos = filter ((/=0).(`mod`10000)) s'152
normals = S.toList $ S.difference (S.fromList s'152) (S.fromList weirdos)
valids = S.fromList $ filter ((==0) . (`mod`10000)) $ map sum $ powerset weirdos

addDenom :: (M.Map Rational Int) -> Int -> (M.Map Rational Int)
addDenom sums denom = M.unionWith (+) sums $ M.mapKeysWith (+) (+ 1%(fromIntegral $ denom^2)) sums
initialSums :: M.Map Rational Int
initialSums = M.fromList [(1%4,1)]
--Still takes just as long
--allSums = foldl' addDenom initialSums [3..80]

kmkk [0] = True
kmkk [_] = False
kmkk s = kmkk $ (abs $ (\([a,b]) -> a - b) $ take 2 l) : drop 2 l
    where l = sortBy (comparing negate) s

--p187
limit187 = 10^8
{-piArray :: forall (a :: * -> * -> *) t a1.
           (Num t, IArray a a1, Ix t, Num a1, Enum a1) =>
           a t a1-}
--piArray :: (IArray a e, Ix i) => a i e
piArray :: UArray Int Int
piArray = listArray (1, limit187) $
          concat $ zipWith replicate (diffs $ 0:primes) [0..]
primeCount = (piArray !)
sublist187 primeTail = let p = head primeTail in
                       takeWhile (<limit187) $ map (*p) primeTail
p187 :: [Integer]
p187 = concatMap sublist187 $
       takeWhile ((<limit187) . head) $ tails primes
--main = print $ length p187

f197 :: Double -> Double
f197 x = fromIntegral (floor (2 ** (30403243784 - x ^ 2))) * 1e-9

--p89
val 'M' = 1000
val 'D' = 500
val 'C' = 100
val 'L' = 50
val 'X' = 10
val 'V' = 5
val 'I' = 1
readNumeral (a:b:s)
    | val a >= val b = (val a) + readNumeral (b:s)
    | val a <  val b = (val b - val a) + readNumeral s
readNumeral [c] = val c
readNumeral "" = 0

numeralAssocs = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
                 (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
                 (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
writeNumeral n = writeNumeral' n numeralAssocs
writeNumeral' n ((v, s) : assocs)
    | n == 0 = ""
    | n >= v = s ++ writeNumeral' (n - v) numeralAssocs
    | otherwise = writeNumeral' n assocs

minimizeNumeral = writeNumeral . readNumeral
                  
p89 = do
  dat <- readFile "roman.txt"
  let numerals = map trim $ lines dat
  let minNumerals = map minimizeNumeral numerals
  print $ sum $ zipWith (-) (map length numerals) (map length minNumerals)

--p103
isSpecialSum set =
    (all (\(b, c) -> sum b > sum c) $
     takeWhile (\(b, c) -> length b + length c <= length set) $
     zip (tail (inits set)) (reverse $ tails set))
    &&
    (trace "tattle" $
     and [sum b /= sum c | i <- [1..length set `div` 2],
                           b <- choose i set,
                           c <- choose i (set \\ b),
                           b /= c])
p103 = head $ sortBy (comparing sum) $ filter isSpecialSum (choose 7 [20..45])
--main = p105

p105 = do
  dat <- readFile "sets.txt"
  let sets = map (sortNub.read.("["++).(++"]").trim) $ lines dat
  print $ sum $ map sum $ filter isSpecialSum sets
  --mapM_ print $ filter isSpecialSum sets

subsetPairs set = [(b,c) | i <- [1..length set],
                           j <- [1..length set],
                           b <- choose i set,
                           c <- choose j (set \\ b),
                           b < c]
subsetNeedsEq (b, c) =
    (length b == length c && length b > 1)
     &&
    (not (b `dominates` c)) && (not (c `dominates` b))

[] `dominates` [] = True
b `dominates` c
  | head c < head b = (tail b) `dominates` (tail c)
  | otherwise = False

p106 = length $ filter subsetNeedsEq $ subsetPairs [1,2,3,4,5,6,7,8,9,10,11,12]