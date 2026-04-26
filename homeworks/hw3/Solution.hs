module Solution where

import Control.Monad (guard, mapM)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup dir =<< Map.lookup pos maze

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = return pos
followPath maze pos (d : ds) = do
  pos' <- move maze pos d
  followPath maze pos' ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = return [pos]
safePath maze pos (d : ds) = do
  pos' <- move maze pos d
  rest <- safePath maze pos' ds
  return (pos : rest)

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = mapM (decrypt key)

type Guest = String

type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations' guests
  guard (not (any (conflictAdjacent perm) conflicts))
  return perm
  where
    conflictAdjacent p (a, b) = adjacentInCircle a b p

adjacentInCircle :: (Eq a) => a -> a -> [a] -> Bool
adjacentInCircle a b xs
  | a == b = length (filter (== a) xs) > 1
  | otherwise = case indices of
      [i, j] -> (j - i) `mod` n == 1 || (i - j) `mod` n == 1
      _ -> False
  where
    n = length xs
    indices = [i | (x, i) <- zip xs [0 ..], x == a || x == b]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = do
  (y, ys) <- select xs
  map (y :) (permutations' ys)

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

data Result a = Failure String | Success a [String] deriving (Show)

instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success x ws) = Success (f x) ws

instance Applicative Result where
  pure x = Success x []
  Failure e <*> _ = Failure e
  Success f ws <*> r = case r of
    Failure e -> Failure e
    Success x ws' -> Success (f x) (ws ++ ws')

instance Monad Result where
  Failure e >>= _ = Failure e
  Success x ws >>= f = case f x of
    Failure e -> Failure e
    Success y ws' -> Success y (ws ++ ws')

warn :: String -> Result ()
warn w = Success () [w]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
  | age < 0 = failure "Age cannot be negative"
  | age > 150 = do
      warn "Age is above 150"
      return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Add e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  case (e1', e2') of
    (Lit 0, _) -> do
      tell ["Add identity: 0 + e -> e"]
      return e2'
    (_, Lit 0) -> do
      tell ["Add identity: e + 0 -> e"]
      return e1'
    (Lit a, Lit b) -> do
      tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
      return (Lit (a + b))
    _ -> return (Add e1' e2')
simplify (Mul e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  case (e1', e2') of
    (Lit 0, _) -> do
      tell ["Zero absorption: 0 * e -> 0"]
      return (Lit 0)
    (_, Lit 0) -> do
      tell ["Zero absorption: e * 0 -> 0"]
      return (Lit 0)
    (Lit 1, _) -> do
      tell ["Mul identity: 1 * e -> e"]
      return e2'
    (_, Lit 1) -> do
      tell ["Mul identity: e * 1 -> e"]
      return e1'
    (Lit a, Lit b) -> do
      tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
      return (Lit (a * b))
    _ -> return (Mul e1' e2')
simplify (Neg e) = do
  e' <- simplify e
  case e' of
    Neg e'' -> do
      tell ["Double negation: -(-e) -> e"]
      return e''
    Lit 0 -> do
      tell ["Neg identity: -0 -> 0"]
      return (Lit 0)
    _ -> return (Neg e')

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- ZipList cannot have a lawful Monad instance because:
-- The join operation would need to handle lists of different lengths.
-- If we have ZipList [ZipList [1,2], ZipList [3,4,5]], what should the result be?
-- The Monad laws would require consistency, but there's no sensible way to merge
-- lists of unequal length while preserving the positional semantics of ZipList.
-- This breaks the associativity law: (m >>= f) >>= g /= m >>= (\x -> f x >>= g)
-- when f and g return lists of different lengths.
