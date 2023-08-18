module Arithmetic (isPrime, Arithmetic.gcd, phi) where

------------------------------------------------------------------------------------------------------------------------------------ Problem 31
-- Description:
--    Determine whether a given integer number is prime.
-- Solution:
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime possiblePrime = checkDivisibility 2
  where
    checkDivisibility n
      | possiblePrime < (n * n) = True
      | possiblePrime `mod` n == 0 = False
      | otherwise = checkDivisibility $ n + 1

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 32:
-- Description:
--    Determine the greatest common divisor of two positive integer numbers.
-- Solution:
gcd :: Integral a => a -> a -> a
gcd a b
  | a < b = if mod b a == 0 then a else Arithmetic.gcd a (b - a)
  | a > b = if mod a b == 0 then b else Arithmetic.gcd (a - b) b
  | otherwise = a

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 33:
-- Description:
--    Determine whether two positive integer numbers are coprime.
-- Solution:
coprime :: Integral a => a -> a -> Bool
coprime a b = Arithmetic.gcd a b == 1

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 34:
-- Description:
--    Calculate Euler's totient function φ(m).
-- Solution:
phi m
  | m < 1 = error "Phi is undefined for numbers less than 1"
  | m == 1 = 1
  | otherwise = phiAux 1 0
  where
    phiAux currentlyChecking totalCoprimes
      | currentlyChecking == m = totalCoprimes
      | otherwise =
          if coprime m currentlyChecking
            then phiAux (currentlyChecking + 1) (totalCoprimes + 1)
            else phiAux (currentlyChecking + 1) totalCoprimes

----------------------------------------------------------------------------------------------------------------------------------
