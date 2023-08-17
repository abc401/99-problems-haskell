module Arithmatic (isPrime) where

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 31
-- Description:
--    Determine whether a given integer number is prime.
-- Solution:
isPrime 1 = False
isPrime possiblePrime = checkDivisibility 2
  where
    -- sqrtN = sqrt n
    checkDivisibility n
      | possiblePrime < (n * n) = True
      | possiblePrime `mod` n == 0 = False
      | otherwise = checkDivisibility $ n + 1

----------------------------------------------------------------------------------------------------------------------------------
