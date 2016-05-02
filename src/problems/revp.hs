import Dna
import qualified Fasta as F

-- | Given a DNA string of length at most 1 kbp in FASTA format.
-- return the position and length of every reverse palindrome in the string
-- having length between 4 and 12. You may return these pairs in any order.
--
-- >>> >Rosalind_24
-- >>> TCAATGCATGCGGGTCTATATGCAT
--
-- 4 6
-- 5 4
-- 6 6
-- 7 4
-- 17 4
-- 18 4
-- 20 6
-- 21 4

revp :: Num t => [Char] -> [(t, Int)]
revp seq = [ (fst x, y) | x <- allSuffix seq 1, y <- [4..12], isRevC (snd x) y]


-- | generates all suffixes for a sequence, indexed by n
allSuffix :: Num n => [t] -> n -> [(n, [t])]
allSuffix []       _ = []
allSuffix q@(r:rs) n = (n,q) : allSuffix rs (n+1)


-- | checks if a sequence is a reverse complement of itself for some length n
isRevC :: [Char] -> Int -> Bool
isRevC seq n =
  let q = take n seq
  in (length q == n) && (q == revc q)


-- copied from revc.hs
revc :: String -> String
revc = show . map Dna.compl . read . reverse


main = do
  inp <- getContents
  let (seq:_) = map F.sequenceData $ F.fastaLines inp
      printPair = (\(a,b) -> putStr (show a) >> putStr " " >> putStrLn (show b))
  mapM_ printPair (revp seq)
