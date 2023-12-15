{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-export-lists #-}

module Main where

-- import qualified Data.List as L

import qualified Control.Monad as M
import System.IO (stdout)
import qualified System.IO as IO

-- Write a program that reads the name of a person and writes a nice message.
-- The input is the name of the person. If the name has an A, 'You belong to
-- Group A!' must be written. Otherwise, 'You belong to Group B!' must be
-- written.

group :: String -> Char
group name
  | 'a' `elem` name || 'A' `elem` name = 'A'
  | otherwise = 'B'

-- You are at a key point on a deserted road. Depending on the number of
-- kilometers you are willing to travel, you have to go to one city or
-- another. Following the table:
--
-- Input:
-- Input is organized in lines. Each line has two elements separated with
-- a whitespace: the name and the km. The last line is special and only
-- contains an asterisk.
--
-- Output:
-- For each individual, print his/her name and the city to which he/she
-- is addressed.

city :: Int -> String
city dist
  | dist < 18 = "Napa"
  | dist < 25 = "Davenport"
  | dist < 30 = "Naperville"
  | dist < 45 = "Phoenix"
  | otherwise = "Carbondale"

prompt :: String -> IO ()
prompt msg = do
  putStr msg
  -- Alternatively, hSetBuffering stdout NoBuffering
  IO.hFlush stdout

-- Write an IO program which will first read a positive integer,
-- say n, and then reads n integers and writes their sum.
-- The program should prompt appropriately for its inputs
-- and explain its output.
--
-- Example:
--   Compute the sum of some numbers.
--   How many numbers? 3
--   Enter a number: 2
--   Enter a number: 5
--   Enter a number: 6
--   The sum of the numbers is: 13

sumOfNumbers :: IO ()
sumOfNumbers = do
  putStrLn "Compute the sum of some numbers."
  prompt "How many numbers? "
  n <- (readLn :: IO Int)
  let ask _ = do
        prompt "Enter a number: "
        readLn :: IO Int
  nums <- mapM ask [1 .. n]
  putStr "The sum of the numbers is: "
  print $ sum nums

-- Write a program which repeatedly reads integers (one per line)
-- until finding a zero value and outputs a sorted version of
-- the inputs read.
--
-- Example:
--   Enter a number (0 to end): 9
--   Enter a number (0 to end): 4
--   Enter a number (0 to end): 5
--   Enter a number (0 to end): 0
--   [4,5,9]

readUntilZero :: IO [Int]
readUntilZero = do
  prompt "Enter a number (0 to end): "
  n <- (readLn :: IO Int)
  if n == 0
    then return []
    else do
      xs <- readUntilZero
      return (n : xs)

-- In this problem, you are going to implement the
-- "number guessing game" in Haskell.
-- Here is an example of how this might work:
--
-- Think of a number between 1 and 100!
-- Is it 50? higher
-- Is it 75? lower
-- Is it 62? lower
-- Is it 56? yes
-- Great, I won!

play :: Int -> Int -> IO ()
play lo hi
  | lo > hi = putStrLn "This is not possible!"
  | otherwise = do
      let mid = (lo + hi) `div` 2
      let ask = do
            prompt ("Is it " ++ show mid ++ "? ")
            getLine
      let valid = (`elem` ["lower", "higher", "yes"])

      -- (MonadPlus IO) mzero throws:
      -- Prelude Control.Monad> mzero
      -- \*** Exception: user error (mzero)
      ans <- M.mfilter valid ask `M.mplus` return ""

      case ans of
        "yes" -> putStrLn "Great, I own!"
        "lower" -> play lo (mid - 1)
        "higher" -> play (mid + 1) hi
        _ -> putStrLn "Answer properly!"

-- validM :: (a -> Bool) -> IO a -> IO a
-- validM valid ask = do
--   ans <- ask
--   if valid ans
--     then return ans
--     else do
--       putStrLn "Answer properly!"
--       validM valid ask

main :: IO ()
main = do
  -- putStrLn "Hello, what's your name?"
  -- name <- getLine
  -- let grp = [group name]
  -- putStrLn $ "You belong to Group " ++ grp ++ "!"
  --

  -- putStrLn "<name> <distance>"
  -- line <- getLine
  -- if line /= "*" then do
  --   let [name, dist] = words line
  --   putStrLn $ name ++ ": " ++ (city (read dist :: Int))
  --   main
  -- else
  --   return ()

  -- sumOfNumbers

  -- nums <- L.sort <$> readUntilZero
  -- print nums

  putStrLn "Think of a number between 1 and 100!"
  play (1 :: Int) 100
