import qualified Fasta as F
import           Rna

-- | Given an RNA string s of length at most 80 bp
-- having the same number of occurrences of 'A' as 'U' and
-- the same number of occurrences of 'C' as 'G'.
-- return the total possible number of perfect matchings of
-- basepair edges in the bonding graph of s
--
-- >>> >Rosalind_23
-- >>> AGCUAGUCAU
-- 
-- > 12
--
-- Insights
-- 1. The 'A -> U' pairs are independent of the 'C -> G' pairs
-- 2. To get number of 'perfect matchings' for n pairs is just n!
-- 3. The total number is `m! * n!` where
--    m = num of 'A -> U' pairs
--    n = num of 'C -> G' pairs

pmch :: [Rna.Base] -> Integer
pmch xs = (howMany Rna.A xs) * (howMany Rna.C xs)
  where howMany n = fact . toInteger . length . filter (== n)
        fact n = product [1..n] -- get factorial n = n!

main :: IO ()
main = do
  inp <- getContents
  let s = F.sequenceData $ head $ F.fastaLines inp
  putStrLn $ show (pmch (Rna.toBases s))
