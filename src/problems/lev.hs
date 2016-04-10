-- | Given six positive integers, each of which does not exceed 20,000.
-- The integers correspond to the number of couples in a population possessing
-- each genotype pairing for a given factor. In order, the six given integers
-- represent the number of couples having the following genotypes
--
-- AA-AA
-- AA-Aa
-- AA-aa
-- Aa-Aa
-- Aa-aa
-- aa-aa
-- 
-- return the expected number of offspring displaying the dominant phenotype
-- in the next generation, under the assumption that every couple has exactly
-- two offspring.
--
-- >>> 1 0 0 1 0 1
-- 3.5

-- | simple calculation based on possible outcome for each type
-- lev :: Fractional a => (a, a, a, a, a, t) -> a
lev (a:b:c:d:e:f:_) = a * 2 + b * 2 + c * 2 + d * 1.5 + e * 1


main = do
  inp <- getLine
  let pairs = map (\a -> read a :: Double) $ words inp
  print $ lev pairs
