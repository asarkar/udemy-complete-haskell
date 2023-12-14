module Section04 (absValue, power, isPrime, fib) where

absValue :: Int -> Int
absValue x = if x >= 0 then x else -x

power :: Int -> Int -> Int
power x n
  | n == 0 = 1
  | x == 0 = 0
  | otherwise = y * (if even n then y else x * y)
  where
    y = power x (n `div` 2)

isPrime :: Int -> Bool
isPrime n
  | n == 2 || n == 3 = True
  | n <= 1 || even n || n `mod` 3 == 0 = False
  | otherwise = all prime [5, 11 .. i]
  where
    i = floor $ sqrt (fromIntegral n :: Float)
    prime j = (n `mod` j) /= 0 && (n `mod` (j + 2)) /= 0

fib :: Int -> Int
fib n
  | n < 2 = n
  | otherwise = fib' 0 1 (n - 2)
  where
    fib' x y k
      | k == 0 = x + y
      | otherwise = fib' y (x + y) (k - 1)
