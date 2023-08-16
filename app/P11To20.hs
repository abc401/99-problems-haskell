{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module P11To20 (encode, decode, encodeDirect, duplicate, P11To20.replicate) where

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

-- Problem 12:
-- Description:
--    Decode a Run-length encoded list
-- Solution:
decode :: [Rle a] -> [a]
decode [] = []
decode xs = rev $ decodeAux xs []
  where
    decodeAux [] acc = acc
    decodeAux (One x : xs) acc = decodeAux xs (x : acc)
    decodeAux (Many (n, x) : xs) acc = decodeAux xs $ repeat x n acc

    repeat x 0 acc = acc
    repeat x n acc = repeat x (n - 1) (x : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 13:
-- Description:
--    Run-length encoding of a list (direct solution)
-- Solution:
encodeDirect :: Eq a => [a] -> [Rle a]
encodeDirect [] = []
encodeDirect (x : xs) = rev $ encodeDirectAux xs (One x) []
  where
    encodeDirectAux [] rle acc = rle : acc
    encodeDirectAux (x : xs) (One x') acc
      | x == x' = encodeDirectAux xs (Many (2, x')) acc
      | otherwise = encodeDirectAux xs (One x) $ One x' : acc
    encodeDirectAux (x : xs) (Many (n, x')) acc
      | x == x' = encodeDirectAux xs (Many (n + 1, x')) acc
      | otherwise = encodeDirectAux xs (One x) $ Many (n, x') : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 14:
-- Description:
--    Duplicate the elements of a list
-- Solution:
duplicate :: [a] -> [a]
duplicate xs = rev $ duplicateAux xs []
  where
    duplicateAux [] acc = acc
    duplicateAux (x : xs) acc = duplicateAux xs $ x : x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 15:
-- Description:
--    Replicate the elements of a list a given number of times.
-- Solution:
replicate xs n = rev $ replicateAux xs n []
  where
    globalTimes = n

    replicateAux [] _ acc = acc
    replicateAux (x : xs) currentTimes acc
      | globalTimes <= 0 = []
      | currentTimes <= 0 = replicateAux xs globalTimes acc
      | otherwise = replicateAux (x : xs) (currentTimes - 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------
