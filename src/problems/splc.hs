import           Data.Foldable ( toList )
import qualified Dna
import qualified Fasta as F
import qualified Protein as P
import qualified Rna

-- | Given a DNA string s (of length at most 1 kbp) and a collection of
-- substrings of s acting as introns, return a protein string resulting
-- from transcribing and translating the exons of s.
-- All strings are given in FASTA format.
--
-- Note: Only one solution will exist for the dataset provided.
--
-- >>> >Rosalind_10
-- >>> ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
-- >>> >Rosalind_12
-- >>> ATCGGTCGAA
-- >>> >Rosalind_15
-- >>> ATCGGTCGAGCGTGT
-- MVYIADKQHVASREAYGHMFKVCA


splc :: String -> String -> String
splc []  _          = []
splc big []         = big
splc big@(b:bs) sml =
  let l      = length sml
      (m, n) = splitAt l big
  in if (m == sml) then (splc n sml) else b:(splc bs sml)


-- | parses text into amino acids
readExons :: String -> [P.AminoAcid]
readExons = (Rna.toAminoAcids . Rna.toCodons . Rna.fromDna . Dna.toBases)


main :: IO ()
main = do
  inp <- getContents
  let (seq:introns)  = (map F.sequenceData . F.fastaLines) inp
      exons          = foldl splc seq introns
  putStrLn $ concat (map show (readExons exons))
