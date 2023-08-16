module P1To10 (P1To10.last, lastTwo, at, P1To10.length, rev, isPalindrome, flatten, compress, pack, encode) where

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 1:
-- Description:
--    Find the last element of a generic list
-- Solution:
last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = P1To10.last xs

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 2:
-- Description:
--    Find the last two elements of a gerneric list
-- Solution:
lastTwo :: [b] -> Maybe (b, b)
lastTwo [] = Nothing
lastTwo [_] = Nothing
lastTwo [x, y] = Just (x, y)
lastTwo (x : xs) = lastTwo xs

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 3:
-- Description:
--    Find the k'th element of a generic list
-- Solution:
at :: Integer -> [a] -> Maybe a
at _ [] = Nothing
at 1 (x : _) = Just x
at n (_ : xs)
  | n < 1 = Nothing
  | otherwise = at (n - 1) xs

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 4:
-- Description:
--    Find length of a list
-- Solution:
length :: [a] -> Integer
length xs = lengthAux xs 0
  where
    lengthAux (_ : xs) n = lengthAux xs (n + 1)
    lengthAux [] n = n

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 5:
-- Description:
--    Reverse a list
-- Solution:
rev :: [a] -> [a]
rev xs = revAux xs []
  where
    revAux [] acc = acc
    revAux (x : xs) acc = revAux xs (x : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 6:
-- Description:
--    Find out whether a list is a palindrome
-- Solution:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindromeAux xs (rev xs)
  where
    isPalindromeAux [] [] = True
    isPalindromeAux (x : xs) (r : rs)
      | x == r = isPalindromeAux xs rs
      | otherwise = False

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 7:
-- Description:
--    Flattening a nested list structure
-- Type definitions:
data Node a = One a | Many [Node a]

-- Solution:
flatten :: [Node a] -> [a]
flatten xs = rev (flattenAux xs [])
  where
    flattenAux [] acc = acc
    flattenAux (One x : xs) acc = flattenAux xs (x : acc)
    flattenAux (Many xs1 : xs2) acc = flattenAux xs2 (flattenAux xs1 acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 8:
-- Description:
--    Eliminate consecutive duplicates of list elements
-- Solution:
compress :: (Eq a) => [a] -> [a]
compress xs = rev (compressAux xs [])
  where
    compressAux [] acc = acc
    compressAux [x] acc = x : acc
    compressAux (x : y : xs) acc
      | x == y = compressAux xs (x : acc)
      | otherwise = compressAux xs (x : y : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 9:
-- Description:
--    Pack consecutive duplicates of list elements into sublists
-- Solution:
pack :: (Eq a) => [a] -> [[a]]
pack xs = rev (packAux xs [] [])
  where
    packAux [] xsAcc totalAcc = xsAcc : totalAcc
    packAux (x : xs) [] totalAcc = packAux xs [x] totalAcc
    packAux (x : xs) (xAcc : xsAcc) totalAcc
      | x == xAcc = packAux xs (x : xAcc : xsAcc) totalAcc
      | otherwise = packAux xs [x] ((xAcc : xsAcc) : totalAcc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 10:
-- Description:
--    Run-length encoding of a list
-- Solution:
encode :: (Eq a) => [a] -> [(Integer, a)]
encode [] = []
encode (x : xs) = rev $ encodeAux xs (1, x) []
  where
    encodeAux [] (n, x') acc = (n, x') : acc
    encodeAux (x : xs) (n, x') acc
      | x == x' = encodeAux xs (n + 1, x') acc
      | otherwise = encodeAux xs (1, x) $ (n, x') : acc

----------------------------------------------------------------------------------------------------------------------------------
