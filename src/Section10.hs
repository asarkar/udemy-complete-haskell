module Section10 (ones, nats, ints, triangulars, factorials, fibs, primes, hammings) where

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of ones [1,1,1...]. Use the function ones :: [Integer].

ones :: [Integer]
ones = 1 : ones -- repeat 1

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of natural numbers [0,1,2...].
-- Use the function nats :: [Integer].

nats :: [Integer]
nats = scanl (+) 0 ones -- [0 ..]

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of integer numbers [0,1,-1,2,-2,3,-3...].
-- Use the function ints :: [Integer].

ints :: [Integer]
-- ints = concat $ zipWith (\x y -> [x, y]) [0, -1 ..] [1 ..]
ints = iterate go 0
  where
    go i = -i + toInteger (fromEnum (i <= 0))

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of triangular numbers [0,1,3,6,10...].
-- Use the function triangulars :: [Integer].
--
-- https://en.wikipedia.org/wiki/Triangular_number

triangulars :: [Integer]
-- triangulars = 0 : L.unfoldr (\(x, y) -> let z = x + y + 1 in Just (z, (z, y + 1))) (0, 0)
triangulars = scanl (+) 0 [1 ..]

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of the factorial numbers [1,1,2,6,24...].
-- Use the function factorials :: [Integer].

factorials :: [Integer]
factorials = scanl (*) 1 [1 ..] -- ans[i] = i!

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of the Fibonacci numbers [0,1,1,2,3,5...].
-- Use the function fibs :: [Integer].

fibs :: [Integer]
-- 0 : 1 : zipWith (+) (0 : ...) (1: ...)
-- 0 : 1 : (0+1) : zipWith (+) (1 : ...) (1: ...)
-- 0 : 1 : 1 : (1+1) : zipWith (+) (1 : ...) (2: ...)
-- 0 : 1 : 1 : 2 : (1+2) : zipWith (+) (2 : ...) (3: ...)
--
-- Ellipses (...) here denote tails that have not been evaluated yet.
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the sequence of the prime numbers [2,3,5,7,11...].
-- Use the function primes :: [Integer].

primes :: [Integer]
primes = primes' [2 ..]
  where
    notDivBy x y = y `mod` x /= 0
    primes' [] = []
    primes' (x : xs) = x : primes' (filter (notDivBy x) xs)

-- The goal of this problem is to work the definition of infinite lists.
-- In particular, you are required to define the function that generates
-- the ordered sequence of the Hamming numbers [1,2,3,4,5,6,8,9...].
-- The Hamming numbers are those that only have 2, 3 and 5 as prime divisors.
-- Use the function hammings :: [Integer].

hammings :: [Integer]
-- hammings
-- = 1 : ...
-- = 1 : merge (2 : ...) (merge (3 : ...) (5 : ...))
-- = 1 : merge (2 : ...) (3 : merge (...) (5 : ...))
-- = 1 : 2 : merge (...) (3 : merge (...) (5 : ...))
-- = 1 : 2 : 3: merge (4 : ...) (merge (...) (5 : ...))
-- = 1 : 2 : 3: 4 : merge (...) (merge (6 : ...) (5 : ...))
--
-- Only the heads need to be resolved for merging.
hammings =
  1
    : merge
      (map (* 2) hammings)
      ( merge
          (map (* 3) hammings)
          (map (* 5) hammings)
      )
  where
    merge xs [] = xs
    merge [] xs = xs
    merge (x : xs) (y : ys)
      | x < y = x : merge xs (y : ys)
      | x > y = y : merge (x : xs) ys
      | otherwise = x : merge xs ys
