{-# OPTIONS_GHC -Wall #-}

module P11To20 (encode, decode, encodeDirect, duplicate, P11To20.replicate, P11To20.drop) where

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
    encodeAux (_x : _xs) (One x') acc
      | _x == x' = encodeAux _xs (Many (2, x')) acc
      | otherwise = encodeAux _xs (One _x) (One x' : acc)
    encodeAux (_x : _xs) (Many (n, x')) acc
      | _x == x' = encodeAux _xs (Many (n + 1, x')) acc
      | otherwise = encodeAux _xs (One _x) (Many (n, x') : acc)

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
    decodeAux (One x : _xs) acc = decodeAux _xs (x : acc)
    decodeAux (Many (n, x) : _xs) acc = decodeAux _xs $ _repeat x n acc

    _repeat _ 0 acc = acc
    _repeat x n acc = _repeat x (n - 1) (x : acc)

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
    encodeDirectAux (_x : _xs) (One x') acc
      | _x == x' = encodeDirectAux _xs (Many (2, x')) acc
      | otherwise = encodeDirectAux _xs (One _x) $ One x' : acc
    encodeDirectAux (_x : _xs) (Many (n, x')) acc
      | _x == x' = encodeDirectAux _xs (Many (n + 1, x')) acc
      | otherwise = encodeDirectAux _xs (One _x) $ Many (n, x') : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 14:
-- Description:
--    Duplicate the elements of a list
-- Solution:
duplicate :: [a] -> [a]
duplicate xs = rev $ duplicateAux xs []
  where
    duplicateAux [] acc = acc
    duplicateAux (x : _xs) acc = duplicateAux _xs $ x : x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 15:
-- Description:
--    Replicate the elements of a list a given number of times.
-- Solution:
replicate :: (Ord p, Num p) => [a] -> p -> [a]
replicate xs n = rev $ replicateAux xs n []
  where
    globalTimes = n

    replicateAux [] _ acc = acc
    replicateAux (x : _xs) currentTimes acc
      | globalTimes <= 0 = []
      | currentTimes <= 0 = replicateAux _xs globalTimes acc
      | otherwise = replicateAux (x : _xs) (currentTimes - 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 16:
-- Description:
--    Drop every N'th element from a list.
-- Solution:
drop :: (Eq p, Num p) => [a] -> p -> [a]
drop xs 0 = xs
drop xs interval = rev $ dropAux xs interval []
  where
    dropAux [] _ acc = acc
    dropAux (_ : _xs) 1 acc = dropAux _xs interval acc
    dropAux (x : _xs) counter acc = dropAux _xs (counter - 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------
