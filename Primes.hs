{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
module Primes
    where

import Random

data Wheel a = (Integral a) => Wheel a [a]
roll (Wheel n rs) = [n*k+r | k <- [0..], r <- rs]
w0 = Wheel 1 [1]
nextSize (Wheel n rs) p =
  Wheel (p*n) [r' | k <- [0..(p-1)], r <- rs, let r' = n*k+r, r' `mod` p /= 0]
mkWheel ds = foldl nextSize w0 ds

--primes :: [Integer]
primes = small ++ large
    where 1:p:candidates = roll $ mkWheel small
          small          = [2,3,5,7]
          large          = p : filter isPrime candidates
          isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) large

a `divides` b = b `mod` a == 0

isPrime :: (Integral a) => a -> Bool
isPrime2 n
    | n <= 1 = False
    | otherwise = millerRabin 30 n
isPrime n
    | n <= 1 = False
    | otherwise = all ((/=0).(n `mod`)) (takeWhile ((<=n).(^2)) primes)
{-
primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]
-}

isComposite = not . isPrime
composites = filter (isComposite) [4..]
nonPrimes = composites
nextPrime n
    | n < 2 = 2
    | odd n = head $ filter isPrime [n+2,n+4..]
    | even n = head $ filter isPrime [n+1,n+3..]

-- These, while nice in theory, are slower than what's above :-(
primesSieve' (p:ns) = p : (primesSieve' $ filter ((/=0) . (`mod`p)) ns)
primesSieve = 2 : primesSieve' [3,5..]

millerRabin1 :: (Integral a, Random a) => StdGen -> a -> (Bool, StdGen)
millerRabin1 g n =
  let (a, g') = randomR (1, n-1) g
  in (millerRabinD a n, g')

millerRabinD :: (Integral a, Random a) => a -> a -> Bool
millerRabinD a n
    | even n = False
    | odd n =
        let (d, s) = sansTwos (n-1) in
        let witness = (a^d `mod` n) /= 1 && (and $ map (\r -> (a^((2^r) * d) `mod` n) /= n-1) [0..s-1])
        in
          --if witness then trace (show a) (not witness) else (not witness)
          not witness
sansTwos n
    | even n = let (d, s) = sansTwos (n `div` 2) in (d, s+1)
    | odd n = (n, 0)

millerRabin :: (Integral a, Random a) => Int -> a -> Bool
millerRabin k n =
    let g = mkStdGen 30132 in millerRabin' g k n
millerRabin' g 0 n = True
millerRabin' g k n =
    let (result, g') = millerRabin1 g n
    in result && millerRabin' g' (k-1) n
