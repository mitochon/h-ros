import           Data.List (intercalate)

-- | Given a DNA string s of length at most 100 bp and an array A containing
-- at most 20 numbers between 0 and 1, return an array B having the same length
-- as A in which B[k] represents the common logarithm of the probability that
-- a random string constructed with the GC-content found in A[k] will match s
-- exactly.
--
-- >>> ACGATACAA
-- >>> 0.129 0.287 0.423 0.476 0.641 0.742 0.783
-- -5.737 -5.217 -5.263 -5.360 -5.958 -6.628 -7.009
--
-- Note:
-- 1. Common logarithm (x) = log_10 (x)
-- 2. Given GC content y, the probability
--    p(G) = p(C) = y/2
--    p(A) = p(T) = (1-y)/2
-- 3. For any positive numbers x and y, log10(x⋅y) = log10(x) + log10(y)

prob :: (Floating b, Ord b, Foldable t) => t Char -> b -> b
prob s gcc = if (gcc > 0) then (foldl px 0 s) else 0
  where gc = gcc / 2
        at = (1 - gcc) / 2
        px = (\acc base -> if (base == 'G' || base == 'C')
                           then acc + (logBase 10 gc)
                           else acc + (logBase 10 at))


main = do
  seq <- getLine
  arr <- getLine
  let gc     = map (\i -> read i :: Float) (words arr)
      probX  = map (prob seq) gc
      clean  = (\a -> if (a == ',') then ' ' else a)
      pprint = map clean . show
  print (pprint probX)


-- from perm.hs
show' :: Show a => [a] -> String
show' = intercalate " " . map show
