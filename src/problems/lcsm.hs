-- | Given a collection of k (k≤100) DNA strings of length at most 1
-- kbp each in FASTA format.
-- Return a longest common substring of the collection.
-- (If multiple solutions exist, you may return any single solution.)
--
-- >Rosalind_1
-- GATTACA
-- >Rosalind_2
-- TAGACCA
-- >Rosalind_3
-- ATACA
--
-- >> AC

import           Data.List
import           Data.Function
import qualified Fasta as F

-- | Mostly copied from
-- https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
-- Slightly modified to account for maxima (plural)
lcsm :: Eq a => [a] -> [a] -> [[a]]
lcsm xs ys = filterMaxLen candidates
  where candidates = concat $ a ++ b
        a          = [f xs' ys | xs' <- tails xs]
        b          = [f xs ys' | ys' <- drop 1 $ tails ys]
        f xs ys    = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then z ++ [x] else []


-- | outer loop for lcsm
outer :: Eq a => [[a]] -> [a] -> [[a]]
outer xs ys = filterMaxLen $ concat [ lcsm x ys | x <- xs ]


-- | Extracts only the longest element(s) from a list
filterMaxLen :: [[a]] -> [[a]]
filterMaxLen xs = filter ((== maxx) . length) xs
  where maxx = maximum $ map length xs


main :: IO()
main = do
  inp <- getContents
  let (fst:rest) = map F.sequenceData $ F.fastaLines inp
  print $ foldl outer [fst] rest
