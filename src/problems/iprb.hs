-- | Given three positive integers k, m, and n, representing a population
-- containing k + m + n organisms, where k individuals are homozygous
-- dominant for a factor, m are heterozygous, and n are homozygous recessive.
-- return the probability that two randomly selected mating organisms will
-- produce an individual possessing a dominant allele
-- (and thus displaying the dominant phenotype).
-- Assume that any two organisms can mate.
--
-- >>> 2 2 2
-- 0.78333
--
-- Some basic observations (where '.' means paired with)
-- (k.k)(k.m)(k.n) -> 100% dominant phenotype
-- (m.m) -> 75% dominant phenotype
-- (m.n) -> 50% dominant phenotype
-- (n.n) -> 0% dominant phenotype
--
-- number of ways to take 2 (a pair) out of X organisms =
-- x * x-1
-- number of ways to pair (a.a) from X organisms =
-- k * k - 1
-- number of ways to pair (a.b) from X organisms =
-- a * b + b * a = 2 * a * b

iprb :: (Fractional a, Ord a) => a -> a -> a -> a
iprb k m n =
  let kk  = pairs k k True
      km  = pairs k m False
      kn  = pairs k n False
      mm  = pairs m m True
      mn  = pairs m n False
      pDominant = (kk + km + kn) + (3/4 * mm) + (1/2 * mn)
      pairings  = pairs (k + m + n) (k + m + n) True
  in pDominant / pairings


-- | number of ways to pair 2 organisms
pairs :: (Num a, Ord a) => a -> a -> Bool -> a
pairs a b sameType
  | (a < 1) || (b < 1) = 0
  | otherwise          = if sameType then (a * (b - 1)) else (2 * a * b)


main = do
  inp <- getContents
  let (a:b:c:_) = words inp
      parseInp i = fromIntegral (read i :: Integer) 
  print $ iprb (parseInp a) (parseInp b) (parseInp c)
