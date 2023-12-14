module Section07 (dupli, insertIn, myButLast, myLast) where

-- Write a function myLast :: [a] -> a that,
-- given a list of elements of type a, returns
-- the last element of the list.

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Write a function myButLast :: [a] -> a that,
-- given a list of elements of type a, returns
-- the penultimate element of the list.

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [_] = error "Singleton list"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Write a function dupli :: [a] -> [a] that
-- duplicates the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- Define a function average :: [Int] -> Float that,
-- given a non-empty list of integers, returns its average.
--
-- Prefixing with _ gets rid of the "unused-top-binds" warning.

_average :: [Int] -> Float
_average xs = sum' / len
  where
    sum' = fromIntegral (sum xs) :: Float
    len = fromIntegral (length xs) :: Float

-- Define a function insertIn :: a -> [a] -> Int -> [a]
-- that inserts an element in a given position into a list.
--
-- Examples:
--   insertIn 8 [1,5,2,7] 3 ==> [1,5,8,2,7]

insertIn :: a -> [a] -> Int -> [a]
insertIn x xs i
  | i <= 0 || i > length xs = error "Invalid position"
  | otherwise = insert xs 1
  where
    insert [] _ = [x]
    insert (y : ys) k
      | k < i = y : insert ys (k + 1)
      | otherwise = x : y : ys
