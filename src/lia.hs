import Control.Monad(replicateM)

-- | Given two positive integers k (k≤7) and N (N≤2k),
-- return the probability that at least N Aa Bb organisms will belong to
-- the k-th generation of the family tree
-- 
-- The 0th generation has genotype Aa Bb
-- Each subsequent generation has 2 children
-- Each child mates with another having type Aa Bb.
-- Don't count the Aa Bb mates at each level.
-- Assume that Mendel's second law holds for the factors.
--
-- >>> 2 1
-- 0.684
--
-- Insight:
-- 1) For k = 1 (1st gen) there are 4 siblings, with the following combinations
--   [[1,1],[1,0],[0,1],[0,0]]
-- where 1 = the event that the sibling is AaBb
--       0 = the event that the sibling is not AaBb
-- P (n>=1) = P (n=1)         + P (n=2)
--          = (2 * p * (1-p)) + (1 * p * p)
--
-- 2) p = 1/4 and seems to be constant across generations
-- Check below, e.g.
--
-- >>> let m = ("Aa","Bb")
-- >>> let x = populate [m] m
-- >>> let y = populate x m
-- >>> ((length . isMatch ("Aa","Bb")) x , (length x))
-- (4,16)
-- >>> ((length . isMatch ("Aa","Bb")) y , (length y))
-- (64,256)
-- >>> ((length . isMatch ("Aa","Bb")) z , (length z))
-- (1024,4096)

-- | probability of having 'AaBb' trait in a generation
p = 0.25


-- | initial implemenatation using brute force in 'siblings' and 'hasN'
-- does not scale on k > 4 -> tries to generate 2^32 element list
lia :: Int -> Int -> [Double]
lia k n =
  let total    = 2 ^ k
      siblings = replicateM total [1,0] -- list all possible outcomes
      hasN     = \x -> count x siblings -- count # elms with desired outcome
      prob j k = (fromIntegral j) * (p ^ k) * ((1-p) ^ (total-k))
  in map (\k -> prob (hasN k) k) [n..total]


-- | counts # outcomes where n siblings having the desired trait
count :: (Eq a, Num a, Foldable t) => a -> [t a] -> Int
count n = length . filter (== n) . map (foldl (+) 0)


-- | binomial coefficients save the day
lia' :: (Integral a, Integral b) => b -> a -> [Double]
lia' k n =
  let total   = 2 ^ k
      coeff c = binom total c          -- coeffs for binomial expansion
      prob  x = fromIntegral (coeff x) * (p ^ x) * ((1-p) ^ (total-x))
  in map prob [n..total]


-- | returns the binomial coefficient of n choose k
binom :: Integral a => a -> a -> a
binom n k = if (k > n) then 0 else fact n `div` (fact k * fact (n-k))
  where fact i = product [1..i]


main = do
  inp <- getLine
  let (k:n:_) = map (\x -> read x :: Integer) $ words inp
      probs   = lia' k n
      sum     = foldl (+) 0 probs
  print probs
  print sum


-- helper functions below to validate

-- | list possible outcomes from 2 alleles
cross :: Ord a => [a] -> [a] -> [[a]]
cross allele1 allele2 = [ if (g1 < g2) then g1:g2:[] else g2:g1:[] |
                          g1 <- allele1, g2 <- allele2 ]


-- | generate a population given a seed
populate :: (Ord a, Ord a1) => [([a], [a1])] -> ([a], [a1]) -> [([a], [a1])]
populate seed (a1,a2) = [ (v1,v2) | g <- seed,
                          v1 <- cross (fst g) a1,
                          v2 <- cross (snd g) a2 ]

-- | filters based on
isMatch :: (Eq a, Eq a1) => (a, a1) -> [(a, a1)] -> [(a, a1)]
isMatch = \(x,y) -> filter (\(a,b) -> a == x && b == y)

