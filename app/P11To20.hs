module P11To20 (encode) where

import P1To10 (rev)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 11:
-- Description:
--    Modified Run-length compression
-- Type Definitions:
data Rle a
  = One a
  | Many (Integer, a)
  deriving (Show, Eq)

-- Solution:
encode :: Eq a => [a] -> [Rle a]
encode [] = []
encode (x : xs) = P1To10.rev $ encodeAux xs (One x) []
  where
    encodeAux [] rle acc = rle : acc
    encodeAux (x : xs) (One x') acc
      | x == x' = encodeAux xs (Many (2, x')) acc
      | otherwise = encodeAux xs (One x) (One x' : acc)
    encodeAux (x : xs) (Many (n, x')) acc
      | x == x' = encodeAux xs (Many (n + 1, x')) acc
      | otherwise = encodeAux xs (One x) (Many (n, x') : acc)

----------------------------------------------------------------------------------------------------------------------------------
