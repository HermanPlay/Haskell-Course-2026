import Distribution.SPDX (LicenseId (OLDAP_2_8, Parity_6_0_0, RPL_1_5), LicenseListVersion (LicenseListVersion_3_10))

-- 1
isPrimeBasic :: Int -> Bool
isPrimeBasic n
  | n <= 1 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (/= 0) [n `mod` k | k <- [2 .. s]]
  where
    s = floor $ sqrt $ fromIntegral n

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 = []
  | otherwise =
      [(p, q) | q <- [1 .. n], p <- [1 .. q], isPrimeBasic p, isPrimeBasic q, p + q == n]

-- 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x : xs) = [(min x y, max x y) | y <- xs, gcd x y == 1]

-- 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n
  | n <= 1 = []
  | otherwise = sieve [2 .. n]

-- isPrime :: Int -> Bool
-- isPrime n
--   | n <= 1 = False
--   | otherwise = last (primesTo n) == n

-- 4

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
  | length (head a) /= length b = []
  | otherwise =
      [[sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]] | j <- [0 .. n - 1]] | i <- [0 .. m - 1]]
  where
    m = length a
    n = length (head b)
    p = length b

-- 5
extract :: [a] -> [(a, [a])]
extract [] = []
extract
  (x : xs) = (x, xs) : [(picked, x : rest) | (picked, rest) <- extract xs]

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [chosen : perms | (chosen, rest) <- extract xs, perms <- permutations (k - 1) rest]

-- 6
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] list1 = list1
merge list1 [] = list1
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | y < x = y : merge ys (x : xs)
  | otherwise = y : merge ys xs

hamming :: [Integer]
hamming = 1 : merge (map (2 *) hamming) (merge (map (3 *) hamming) (map (5 *) hamming))

-- 7
power :: Int -> Int -> Int
power 0 _ = 0
power 1 _ = 1
power b e = go b e 1
  where
    go b 0 n = n
    -- go b e n = let acc = n * b in seq acc (go b (e - 1) acc)
    go b e !n = go b (e - 1) (n * b)

-- 8

-- listMax :: [Int] -> Int
-- listMax list = go list (head list)
--   where
--     go [] accMax = accMax
--     go (x : xs) !accMax
--       | x > accMax = go xs x
--       | otherwise = go xs accMax

listMax :: [Int] -> Int
listMax list = go list (head list)
  where
    go [] n = n
    go (x : xs) n = let accMax = max n x in seq accMax (go xs accMax)

-- 9
primes :: [Int]
primes = sieve [2 ..]

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = n `elem` takeWhile (<= n) primes

-- 10
-- a) UNCOMMENT TO SEE
--
-- mean :: [Double] -> Double
-- mean xs = go xs 0 0
--   where
--     go [] s n = s / n
--     go (x : xs) s n = go xs (s + x) (n + 1)
--
-- b)
mean :: [Double] -> Double
mean xs = go xs 0 0
  where
    go [] s n = s / n
    go (x : xs) !s !n = go xs (s + x) (n + 1)

-- c>
--
genericMean :: [Double] -> (Double, Double)
genericMean xs = go xs 0 0 0
  where
    -- s1 is sum of x, s2 is sum of x squared, n is count
    go [] !s1 !s2 !n
      | n == 0 = (0, 0)
      | otherwise =
          let mu = s1 / n
           in (mu, (s2 / n) - (mu * mu))
    go (x : xs) !s1 !s2 !n = go xs (s1 + x) (s2 + x * x) (n + 1)
