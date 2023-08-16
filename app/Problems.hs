{-# OPTIONS_GHC -Wall #-}

module Problems
  ( Problems.last,
    lastTwo,
    at,
    Problems.length,
    rev,
    isPalindrome,
    flatten,
    compress,
    pack,
    encode,
    encode2,
    decode,
    encodeDirect,
    duplicate,
    Problems.replicate,
    Problems.drop,
    split,
    slice,
    rotate,
    removeAt,
    insertAt,
    range,
  )
where

------------------------------------------------------------------------------------------------------------------------------------ Problem 1:
-- Description:
--    Find the last element of a generic list
-- Solution:
last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = Problems.last xs

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 2:
-- Description:
--    Find the last two elements of a gerneric list
-- Solution:
lastTwo :: [b] -> Maybe (b, b)
lastTwo [] = Nothing
lastTwo [_] = Nothing
lastTwo [x, y] = Just (x, y)
lastTwo (_ : xs) = lastTwo xs

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 3:
-- Description:
--    Find the k'th element of a generic list
-- Solution:
at :: (Num idx, Ord idx) => idx -> [a] -> Maybe a
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
length :: Num length => [a] -> length
length xs = lengthAux xs 0
  where
    lengthAux (_ : _xs) n = lengthAux _xs (n + 1)
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
    revAux (x : _xs) acc = revAux _xs (x : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 6:
-- Description:
--    Find out whether a list is a palindrome
-- Solution:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindromeAux xs (rev xs)
  where
    isPalindromeAux [] [] = True
    isPalindromeAux (x : _xs) (r : rs)
      | x == r = isPalindromeAux _xs rs
    isPalindromeAux _ _ = False

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
    flattenAux (One x : _xs) acc = flattenAux _xs (x : acc)
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
    compressAux (x : y : _xs) acc
      | x == y = compressAux _xs (x : acc)
      | otherwise = compressAux _xs (x : y : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 9:
-- Description:
--    Pack consecutive duplicates of list elements into sublists
-- Solution:
pack :: (Eq a) => [a] -> [[a]]
pack xs = rev (packAux xs [] [])
  where
    packAux [] xsAcc totalAcc = xsAcc : totalAcc
    packAux (x : _xs) [] totalAcc = packAux _xs [x] totalAcc
    packAux (x : _xs) (xAcc : xsAcc) totalAcc
      | x == xAcc = packAux _xs (x : xAcc : xsAcc) totalAcc
      | otherwise = packAux _xs [x] ((xAcc : xsAcc) : totalAcc)

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
    encodeAux (_x : _xs) (n, x') acc
      | _x == x' = encodeAux _xs (n + 1, x') acc
      | otherwise = encodeAux _xs (1, _x) $ (n, x') : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 11:
-- Description:
--    Modified Run-length compression
-- Type Definitions:
data Rle a
  = OneRle a
  | ManyRle (Integer, a)
  deriving (Show, Eq)

-- Solution:
encode2 :: Eq a => [a] -> [Rle a]
encode2 [] = []
encode2 (x : xs) = rev $ encodeAux xs (OneRle x) []
  where
    encodeAux [] rle acc = rle : acc
    encodeAux (_x : _xs) (OneRle x') acc
      | _x == x' = encodeAux _xs (ManyRle (2, x')) acc
      | otherwise = encodeAux _xs (OneRle _x) (OneRle x' : acc)
    encodeAux (_x : _xs) (ManyRle (n, x')) acc
      | _x == x' = encodeAux _xs (ManyRle (n + 1, x')) acc
      | otherwise = encodeAux _xs (OneRle _x) (ManyRle (n, x') : acc)

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
    decodeAux (OneRle x : _xs) acc = decodeAux _xs (x : acc)
    decodeAux (ManyRle (n, x) : _xs) acc = decodeAux _xs $ _repeat x n acc

    _repeat _ 0 acc = acc
    _repeat x n acc = _repeat x (n - 1) (x : acc)

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 13:
-- Description:
--    Run-length encoding of a list (direct solution)
-- Solution:
encodeDirect :: Eq a => [a] -> [Rle a]
encodeDirect [] = []
encodeDirect (x : xs) = rev $ encodeDirectAux xs (OneRle x) []
  where
    encodeDirectAux [] rle acc = rle : acc
    encodeDirectAux (_x : _xs) (OneRle x') acc
      | _x == x' = encodeDirectAux _xs (ManyRle (2, x')) acc
      | otherwise = encodeDirectAux _xs (OneRle _x) $ OneRle x' : acc
    encodeDirectAux (_x : _xs) (ManyRle (n, x')) acc
      | _x == x' = encodeDirectAux _xs (ManyRle (n + 1, x')) acc
      | otherwise = encodeDirectAux _xs (OneRle _x) $ ManyRle (n, x') : acc

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
replicate :: (Ord times, Num times) => [a] -> times -> [a]
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
drop :: (Eq interval, Num interval) => [a] -> interval -> [a]
drop xs 0 = xs
drop xs interval = rev $ dropAux xs interval []
  where
    dropAux [] _ acc = acc
    dropAux (_ : _xs) 1 acc = dropAux _xs interval acc
    dropAux (x : _xs) counter acc = dropAux _xs (counter - 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 17:
-- Description:
--    Split a list into two parts; The length of the first part is given.
-- Solution:
split :: (Eq size, Num size) => [a] -> size -> ([a], [a])
split xs size = splitAux xs 1 []
  where
    splitAux [] _ acc = (rev acc, [])
    splitAux (x : _xs) currentSize acc
      | currentSize == size = (rev $ x : acc, _xs)
      | otherwise = splitAux _xs (currentSize + 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 18:
-- Description:
--     Extract a slice from a list.
-- Solution:
slice :: (Ord bound, Num bound) => [a] -> bound -> bound -> [a]
slice xs lowerBound upperBound = rev $ sliceAux xs 0 []
  where
    sliceAux [] _ acc = acc
    sliceAux (x : _xs) currentIdx acc
      | currentIdx < lowerBound = sliceAux _xs (currentIdx + 1) acc
      | currentIdx > upperBound = acc
      | otherwise = sliceAux _xs (currentIdx + 1) $ x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problems 19:
-- Description:
--    Rotate a list N places to the left.
-- Solution:
rotate :: [a] -> Integer -> [a]
rotate xs amount
  | amount > 0 = combine $ Problems.split xs $ amount `mod` lenXS
  | amount < 0 = combine $ Problems.split xs $ lenXS - (-amount) `mod` lenXS
  | otherwise = xs
  where
    lenXS = Problems.length xs
    combine (a, b) = b ++ a

----------------------------------------------------------------------------------------------------------------------------------

-- Problems 20:
-- Description:
--    Remove the K'th element from a list.
-- Solution:
removeAt :: (Eq index, Num index) => index -> [a] -> [a]
removeAt idx xs = rev $ removeAtAux idx xs []
  where
    removeAtAux _ [] acc = acc
    removeAtAux _idx (x : _xs) acc
      | _idx == 0 = removeAtAux (_idx - 1) _xs acc
      | otherwise = removeAtAux (_idx - 1) _xs $ x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 21:
-- Description:
--    Insert an element at a given position into a list.
-- Solution:
insertAt :: (Ord idx, Num idx) => p -> idx -> [p] -> [p]
insertAt x idx xs
  | idx >= 0 = rev $ insertAtAux 0 xs []
  | otherwise = error "Cannot insert at negative index!"
  where
    insertAtAux _idx [] acc
      | _idx <= idx = x : acc
      | otherwise = acc
    insertAtAux _idx (_x : _xs) acc
      | _idx == idx = insertAtAux (_idx + 1) _xs $ _x : x : acc
      | otherwise = insertAtAux (_idx + 1) _xs $ _x : acc

----------------------------------------------------------------------------------------------------------------------------------

-- Problem 22:
-- Description:
--    Create a list containing all integers within a given range.
-- Solution:
range :: (Ord a, Num a) => a -> a -> [a]
range bound1 bound2 = rangeAux bound1 bound2 []
  where
    delta
      | bound1 > bound2 = 1
      | otherwise = -1
    rangeAux bound1' bound2' acc
      | bound1' == bound2' - delta = acc
      | otherwise = rangeAux bound1' (bound2' + delta) $ bound2' : acc

----------------------------------------------------------------------------------------------------------------------------------
