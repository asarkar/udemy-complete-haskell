{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-orphans #-}

module Section23
  ( fsmap,
    rational,
    numerator,
    denominator,
    multEq,
    selectFirst,
    empty,
    get,
    set,
    inside,
    moves,
    canGo3',
    avg,
    cat,
    maximum,
  )
where

import Control.Arrow ((***), (>>>))
import qualified Data.List as L
import Tree (Tree (..))
import Prelude hiding (Rational, maximum)

-- Define a function fsmap a -> [a -> a] -> a which,
-- given an element x of type a and a list fs of functions
-- of type a -> a, causes fsmap x fs to return the application
-- (from left right) of all functions from fs to x.
--
-- Example:
--   fsmap 3 [(+2), (*3), (+4)] ==> 19

fsmap :: a -> [a -> a] -> a
--- (>>>) == flip (.)
fsmap x fs = foldl (>>>) id fs x

-- Define a Rational type to manipulate positive
-- rational numbers with operations by:
-- 1. Construct a rational through a natural numerator and denominator.
-- 2. Obtain the numerator of its simplified form.
-- 3. Obtain the denominator of its simplified form.
-- Also, make Rational a member of class Eq and class Show,
-- making rationals display in the form "x/y".

data Rational = Rational {numerator :: Integer, denominator :: Integer} deriving stock (Eq)

instance Show Rational where
  show (Rational x y) = show x ++ "/" ++ show y

rational :: Integer -> Integer -> Rational
rational x y = Rational (x `div` z) (y `div` z)
  where
    z = gcd x y

-- Write a function multEq :: Int -> Int -> [Int] that,
-- given two nonzero positive integers x and y, outputs
-- the infinitely-increasing-ordered list containing the
-- numbers formed by multiplying the same amount of x and y.

multEq :: Int -> Int -> [Int]
multEq x y = iterate (z *) 1
  where
    z = x * y

-- Write a function selectFirst :: [Int] -> [Int] -> [Int] -> [Int] that,
-- given three lists l1, l2 and l3, returns the elements of l1 that appear
-- in l2 in a position strictly smaller than in l3. If an element appears
-- in l2 and not in l3, it is considered to appear in a previous position.

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 = filter go l1
  where
    go x = case L.elemIndex x l2 of
      Just i -> null ix || L.any (> i) ix
      _ -> False
      where
        ix = L.elemIndices x l3

-- Consider a generic symbol table that converts texts (Strings) to
-- values of type a defined by
-- type SymTab a = String -> Maybe a
--
-- The symbol table returns a Maybe a and not an a in order to indicate
-- unsuccessful searches.
--
-- The operations on the symbol table are:
-- empty :: SymTab a
-- get :: SymTab a -> String -> Maybe a
-- set :: SymTab a -> String -> a -> SymTab a
--
-- where empty creates an empty symbol table, get returns the value of a
-- text to the symbol table (with Just if present or Nothing if not), and
-- set returns a new symbol table defining a new value for the symbol
-- (and overwriting the old value if the symbol was already in the table).
--
-- Implement these three operations on the given type (which you can't change).

type SymTab a = String -> Maybe a

empty :: SymTab a
empty = const Nothing

get :: SymTab a -> String -> Maybe a
get = id

set :: SymTab a -> String -> a -> SymTab a
set tbl k v x = if k == x then Just v else tbl x

-- Consider a knight on an empty 8x8 chess board. Its position can be given
-- with a tuple indicating its row and column:
-- type Pos = (Int, Int) -- bottom-left box is (1, 1)
--
-- Remember that knights move in an "L":
--
-- 1. Define a function inside :: Pos -> Bool that, given a position of a horse,
--    returns if it is inside the board.
-- 2. Define a function moves :: Pos -> [Pos] that, given a position of a
--    horse within the board, returns the list of positions within the board
--    where it can be found after a jump. The order of the list is not important.
-- 3. Define a function canGo3 :: Pos -> Pos -> Bool that, given a start position
--    p within the board and a final position q, tells whether a horse can go from
--    p to q in (exactly) three jumps.
-- 4. Now define a function canGo3' :: Pos -> Pos -> Bool that does the same as
--    canGo3 but taking advantage of the fact that lists are Monad instances.

type Pos = (Int, Int) -- bottom-left box is (1, 1)

inside :: Pos -> Bool
inside (x, y) = inRng x && inRng y
  where
    inRng i = i >= 1 && i <= 8

moves :: Pos -> [Pos]
moves (x, y) = filter inside $ pos (pdt a b) ++ pos (pdt b a)
  where
    a = [2, -2]
    b = [1, -1]
    pdt xs ys = (,) <$> xs <*> ys
    -- (***)	\f g (x, y) -> (f x, g y)
    -- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows#Arrow_combinators_crop_up_in_unexpected_places
    pos = map ((x +) *** (y +))

canGo3' :: Pos -> Pos -> Bool
canGo3' src dest = dest `elem` (moves src >>= moves >>= moves)

-- The goal of this exercise is to make the type of the binary trees an
-- instance of the Foldable class.
-- It is requested:
-- 1. Make Tree an instance of Foldable. To do this, implement the foldr
--    function by applying a function to the elements of the tree while
--    performing a preorder traversal.
-- 2. Define a function avg :: Tree Int -> Double to calculate the average
--    of the elements of a non-empty tree of integers. Use fromIntegral to
--    convert from integer to real.
-- 3. Define a function cat :: Tree String -> String to concatenate with
--    spaces all the nodes of a text tree.

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Node x l r) = f x (foldr f (foldr f z r) l)

avg :: Tree Int -> Double
avg t = toD total / toD count
  where
    toD :: Int -> Double
    toD i = fromIntegral i :: Double
    (total, count) = foldr (\x (a, b) -> (x + a, b + 1)) (0, 0) t

cat :: Tree String -> String
cat = unwords . foldr (:) []

maximum :: (Ord a) => Tree a -> a
maximum Empty = error "Empty tree"
maximum (Node x l r) = max (foldr max x l) (foldr max x r)
