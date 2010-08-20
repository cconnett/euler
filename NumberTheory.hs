{-# LANGUAGE NoMonomorphismRestriction #-}

module NumberTheory
    where

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Factor
import Primes
import Euler

totient n = product $ map (\(p:ps) -> (p-1)*(product ps)) $ group $ factor n
relPrime x y = gcd x y == 1

coprime m n = (==1) . gcd

divisors 0 = []
divisors 1 = [1]
divisors n = divisors' (group $ factor n)

divisors' [] = [1]
divisors' factors = sort $ nub $ do
  alpha <- [0..length (head factors)]
  map (*(head (head factors)^alpha)) (divisors' $ (tail factors))

tau n = product $ map ((+1) . length) $ group $ factor n
numDivisors = tau

properDivisors 0 = []
properDivisors n = init $ divisors n

improperFactorings n = improperFactorings' n ++ [[n]]
improperFactorings' n
    | isPrime n = [[n]]
    | otherwise = do
  d <- tail $ properDivisors n
  sortNub $ map sort $ map (d:) $
          filter (all (<=d)) $
          improperFactorings (n`div`d)

multInverse m a = (snd $ multInverse' m a) `mod` m
multInverse' a b
    | a `mod` b == 0 = (0, 1)
    | otherwise = let (x, y) = multInverse' b (a `mod` b)
                  in (y, (x - y*(a`div`b)))

perfSquare :: Integer -> Bool
perfCube = perfPower 3
perfPower :: Integer -> Integer -> Bool
--perfPower k n = all ((==k) . genericLength) $ group $ factor n
{-perfPower k n = (fromIntegral $ round $ n'**(1/k'))**k' == n'
    where k' = fromIntegral k :: Double
          n' = fromIntegral n :: Double
-}
perfPower k n = let r = integerRoot k n in n == r^k
integerRoot k 0 = 0
integerRoot k 1 = 1
integerRoot k n =
    fst $ head $ dropWhile (\(a,b) -> a/=b) $
    zip estimates (tail estimates)
    where estimates = iterate (improveEstimate k n) (n`div`k)
improveEstimate k n estimate =
    let slope x = k*x^(k-1) in
    estimate - ((estimate^k - n) `roundingDiv` slope estimate)
--roundingDiv :: Integer -> Integer -> Integer
roundingDiv n d = n `div` d + (fromIntegral $ fromEnum (n `mod` d >= d `div` 2))

quadraticResidues m = S.fromList $ map ((`mod`m) . (^2)) [0..m`div`2]
residues256 = quadraticResidues 256
residues9 = quadraticResidues 9
residues5 = quadraticResidues 5
residues7 = quadraticResidues 7
residues13 = quadraticResidues 13
residues17 = quadraticResidues 17
residues97 = quadraticResidues 97

perfSquare n =
    (n `mod` 256) `S.member` residues256 &&
    (n `mod` 9)   `S.member` residues9 &&
    (n `mod` 5)   `S.member` residues5 &&
    (n `mod` 7)   `S.member` residues7 &&
    (n `mod` 13)  `S.member` residues13 &&
    (n `mod` 17)  `S.member` residues17 &&
    (n `mod` 97)  `S.member` residues97 &&
    perfPower 2 n
