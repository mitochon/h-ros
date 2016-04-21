import           Data.Foldable ( toList )
import           Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LI
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


splc :: Text -> Text -> Text
splc big small
  | L.null big   = L.empty
  | L.null small = big
  | otherwise    =
    let l = L.length small
        h = L.head big
        t = L.tail big
        (m, n) = L.splitAt l big
    in if (m == small) then (splc n small)
       else L.cons h (splc t small)


-- | parses text into amino acids
readExons :: Text -> [P.AminoAcid]
readExons = (Rna.toAminoAcids . Rna.toCodons .  Rna.fromDna . Dna.toBases)


-- | extracts sequences (sans id) from a fasta file
seqList :: Text -> [[Text]]
seqList = map (toList. snd) . toList . F.fastaLines


main :: IO ()
main = do
  inp <- LI.getContents
  let (x:xs)  = seqList inp
      seq     = L.concat x
      introns = map L.concat xs
      exons   = foldl splc seq introns
  putStrLn $ concat (map show (readExons exons))
