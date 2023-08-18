module Arithmetic (isPrime, Arithmetic.gcd) where

----------------------------------------------------------------------------------------------------------------------------------
-- Problem 31
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
gcd a b
  | a < b = if mod b a == 0 then a else Arithmetic.gcd a (b - a)
  | a > b = if mod a b == 0 then b else Arithmetic.gcd (a - b) b
  | otherwise = a

----------------------------------------------------------------------------------------------------------------------------------
