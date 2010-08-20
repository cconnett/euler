module Factor
    where

import Primes

factor :: (Integral k) => k -> [k]
factor 2 = [2]
factor n
    | null $ primesDividing n = [n]
    | otherwise = firstDivisor:(factor (n`div`firstDivisor))
    where firstDivisor = head $ primesDividing n
          primesDividing n = filter ((==0).(n`mod`)) (takeWhile ((<=n).(^2)) primes)
