import qualified Dna
import qualified Fasta as F
import qualified Rna
import qualified Protein as P

-- | Given a DNA string ss of length at most 1 kbp in FASTA format.
-- return every distinct candidate protein string that can be translated
-- from ORFs of ss. Strings can be returned in any order.
--
-- >>> >Rosalind_99
-- >>> AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG
--
-- MLLGSFRLIPKETLIQVAGSSPCNLS
-- M
-- MGMTPRLGLESLLE
-- MTPRLGLESLLE
--
-- Note: 'M' is the 'Start' codon

orf :: [Dna.Base] -> [[P.AminoAcid]]
orf xs =
  let sx     = (map Dna.compl . reverse) xs
      frames = [id, drop 1, drop 2] <*> [xs, sx] -- all 6 reading frames
      codons = readExons <$> frames
   in codons >>= splitBySuffix P.Stop >>= introspect P.M


-- | groups a list by some suffix
-- e.g. 'A' -> "XYZAMNOABCD" -> ["XYZ","MNO"]
splitBySuffix :: Eq a => a -> [a] -> [[a]]
splitBySuffix k xs = (reverse . map reverse . snd) $ foldl tokenize ([],[]) xs
  where tokenize (u, vs) n = if (n == k) then ([], u:vs) else (n:u, vs)


-- | searches the list for additional matches, e.g. "ABAC" -> 'A' -> ["AC", "ABAC"]
introspect :: Eq a => a -> [a] -> [[a]]
introspect y xs =
  let indexedPairs = filter ((== y) . fst) $ zip xs [0..]
      dropIndex    = map snd indexedPairs
  in foldl (\acc i -> (drop i xs) : acc) [] dropIndex


-- mostly copied from splc.hs
readExons :: [Dna.Base] -> [P.AminoAcid]
readExons = (Rna.toAminoAcids . Rna.toCodons . Rna.fromDna)


main = do
  inp <- getContents
  let (seq:_)  = map F.sequenceData $ F.fastaLines inp
      proteins = (concat . (map show)) <$> orf (Dna.toBases seq)
  mapM_ putStrLn proteins
