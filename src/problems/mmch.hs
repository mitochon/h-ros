import qualified Fasta as F
import           Rna

-- | Given an RNA string s of length at most 100 bp, return
-- the total possible number of maximum matchings of basepair
-- edges in the bonding graph of s
--
-- >>> >Rosalind_92
-- >>> AUGCUUC
-- 
-- > 6
--
-- Insights
-- 1. Similar to `pmch`, the 'A -> U' pairs are independent of the
--    'C -> G' pairs
-- 2. To get number of 'perfect matchings' is m!/(m-n)! * a!/(a-b)!
--    where m, n are # 'A -> U' and  m > n > 1
--          a, b are # 'G -> C' and  a > b > 1 (order not important)

mmch :: [Rna.Base] -> Integer
mmch xs =
  let howMany b = toInteger $ length $ filter (== b) xs
      combo m n = product [(m - n + 1)..m] -- equals  m!/(m-n)!
      pair x y  = if (x > y) then combo x y else combo y x
      auPairs   = pair (howMany Rna.A) (howMany Rna.U)
      gcPairs   = pair (howMany Rna.G) (howMany Rna.C)      
  in auPairs * gcPairs


main :: IO ()
main = do
  inp <- getContents
  let s = F.sequenceData $ head $ F.fastaLines inp
  putStrLn $ show (mmch (Rna.toBases s))
