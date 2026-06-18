import Data.Foldable
import Distribution.SPDX (LicenseId (Parity_7_0_0))
import Distribution.Simple.Setup (falseArg)
import System.Posix (accessModes)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show)

-- 1
instance Functor Sequence where
  -- Identity fmap id == id
  fmap _ Empty = Empty
  fmap f (Single a) = Single (f a)
  fmap f (Append sequence1 sequence2) = Append (fmap f sequence1) (fmap f sequence2)

-- 2
instance Foldable Sequence where
  foldMap _ Empty = mempty
  foldMap m (Single a) = m a
  foldMap m (Append s1 s2) = foldMap m s1 <> foldMap m s2

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- 3
instance Semigroup (Sequence a) where
  Empty <> ys = ys
  (Single a) <> ys = Append (Single a) ys
  (Append s1 s2) <> ys = Append s1 (s2 <> ys)

instance Monoid (Sequence a) where
  mempty = Empty

-- 4
tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem x seq = go [seq]
  where
    -- stack is empty
    go [] = False
    -- we have empty
    go (Empty : rest) = go rest
    -- we have singl
    go (Single a : rest)
      | x == a = True
      | otherwise = go rest
    -- we have append
    go (Append s1 s2 : rest) = go (s1 : s2 : rest)

-- 5
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single a : rest) acc = go rest (a : acc)
    go (Append s1 s2 : rest) acc = go (s2 : s1 : rest) acc

-- 6
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN seq = go seq []
  where
    go [] [result] = Just result
    go [] _ = Nothing
    go (TNum x : rest) stack = go rest (x : stack)
    go (TAdd : rest) (y : x : stack) = go rest (x + y : drop 2 stack)
    go (TSub : rest) (y : x : stack) = go rest (x - y : stack)
    go (TMul : rest) (y : x : stack) = go rest (x * y : stack)
    go (TDiv : rest) (y : x : stack)
      | y == 0 = Nothing
      | otherwise = go rest ((x `div` y) : stack)
    go _ _ = Nothing

-- 7
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile predicate = foldr (\x acc -> if predicate x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- 8
encode :: (Eq a) => [a] -> [(a, Int)]
encode = foldr go []
  where
    go x [] = [(x, 1)]
    go x ((y, count) : rest)
      | x == y = (y, count + 1) : rest
      | otherwise = (x, 1) : (y, count) : rest

decode :: [(a, Int)] -> [a]
decode = foldr go []
  where
    go (x, count) acc = replicate count x ++ acc
