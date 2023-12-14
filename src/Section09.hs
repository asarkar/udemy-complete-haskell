module Section09
  ( eql,
    prod,
    prodEvens,
    powersOf2,
    scalarProduct,
    flatten,
    myLength,
    myReverse,
    firstWord,
    countIf,
    combined,
    consecutive,
    filterFoldl,
  )
where

-- Implement a function eql :: [Int] -> [Int] -> Bool that
-- tells whether two lists of integers are equal.

eql :: [Int] -> [Int] -> Bool
eql xs ys
  | length xs /= length ys = False
  | otherwise = all (uncurry (==)) $ zip xs ys

-- Implement a function prod :: [Int] -> Int that returns
-- the product of a list of integers.

prod :: [Int] -> Int
{- HLINT ignore "Use product" -}
prod = foldr (*) 1

-- Implement a function prodEvens :: [Int] -> Int that returns
-- the product of all even numbers of a list of integers.

prodEvens :: [Int] -> Int
prodEvens = prod . filter even

-- Implement a function powersOf2 :: [Int] that generates the
-- list of all the powers of 2.

powersOf2 :: [Int]
powersOf2 = iterate (* 2) 1

-- Implement a function scalarProduct :: [Float] -> [Float] -> Float
-- that returns the dot product of two lists of float numbers with
-- the same size.

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs = sum . zipWith (*) xs

-- Implement a function flatten :: [[Int]] -> [Int] that flattens a
-- list of lists of integers in a list of integers.

{- HLINT ignore "Use concat" -}
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- Implement a function myLength :: String -> Int that returns the
-- length of a string.

myLength :: [a] -> Int
-- (succ (succ 0))...
myLength = foldr (const succ) 0

-- Implement a funtion myReverse :: [Int] -> [Int] that reverses
-- a list of integers.

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Implement a function countIn :: [[Int]] -> Int -> Int that,
-- given a list of sublists l and an element x, returns the list
-- that tells how many times x appears in each sublist of l.

_countIn :: (Eq a) => [[a]] -> a -> [Int]
_countIn xs x = map count xs
  where
    count = length . filter (x ==)

-- Implement a function firstWord :: String -> String that,
-- given a string with blanks and alphabetic characters,
-- return its first word.

firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (== ' ')

-- Define a function countIf :: (Int -> Bool) -> [Int] -> Int that,
-- given a predicate on integers and a list of integers, returns the
-- number of elements in the list that satisfy the predicate.

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

-- Define a function combined :: [Int] -> [Int -> Int] -> [[Int]] that,
-- given a list of integers and a list of functions from integers to
-- integers, returns the list consisting of applying each of the functions
-- in the second list to the elements in the first list.
--
-- Examples:
--   combined [1,2,3] [(+1),(*2),(^2)] ==> [[2,3,4],[2,4,6],[1,4,9]]

combined :: [a] -> [a -> a] -> [[a]]
combined xs = map (`map` xs)

-- Define a function consecutive :: [Int] -> [Int -> Int] -> [[Int]] that,
-- given a list of integers and a list of functions from integers to
-- integers, returns the list of lists where each list is the result of
-- applying, one after the other, the function in the second list to each
-- element in the first list.
--
-- Examples:
--   consecutive [1,2,3] [(+1),(*2),(^2)] ==> [[2,2,1],[3,4,4],[4,6,9]]

consecutive :: [a] -> [a -> a] -> [[a]]
-- id :: a -> a
-- Replacing a by a -> b, we get
--   id :: (a -> b) -> (a -> b)
-- Because of currying it is the same as
--   id :: (a -> b) -> a -> b
-- Applying flip, we get
--   flip id :: a -> (a -> b) -> b
-- Alternatively, ($) :: (a -> b) -> a -> b
--   flip id = flip ($)
consecutive xs fs = map (flip map fs . flip id) xs

-- Define a function filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
-- that returns a fold of all the elements that satisfy the given predicate.

filterFoldl :: (b -> Bool) -> (a -> b -> a) -> a -> [b] -> a
filterFoldl p f x = foldl f x . filter p
