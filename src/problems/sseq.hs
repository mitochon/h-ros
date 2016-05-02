import           Data.List (intercalate)
import qualified Fasta as F

-- | Given two DNA strings s and t (each of length at most 1 kbp) in FASTA
-- format, return one collection of indices of s in which the symbols
-- of t appear as a subsequence of s.
-- If multiple solutions exist, you may return any one.
--
-- >>> >Rosalind_14
-- >>> ACGTACGTGACG
-- >>> >Rosalind_18
-- >>> GTA
-- 3 8 10


sseq :: (Eq a, Num t) => [a] -> [a] -> t -> [t]
sseq [] _  _ = []
sseq _  [] _ = []
sseq s@(a:as) t@(b:bs) n
  | a == b    = n : sseq as bs (n+1)
  | otherwise = sseq as t (n+1)


main = do
  inp <- getContents
  let (s1:s2:_) = map F.sequenceData $ F.fastaLines inp
      ix        = sseq s1 s2 1
      noComma   = putStrLn . intercalate " " . map show
  if (length ix) < (length s2)
    then print "none" >> noComma ix
    else noComma ix
