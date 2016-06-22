import qualified Dna
import qualified Fasta as F
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.List (find)
import           Control.Monad (msum)

-- | As is the case with point mutations, the most common type of sequencing
-- error occurs when a single nucleotide from a read is interpreted
-- incorrectly.
--
-- Given a collection of up to 1000 reads of equal length (at most 50 bp) in
-- FASTA format. Some of these reads were generated with a single
-- nucleotide error.
--
-- For each read s in the dataset, one of the following applies:
-- * s was correctly sequenced and appears in the dataset at least twice
--   (possibly as a reverse complement);
-- * s is incorrect, it appears in the dataset exactly once, and its Hamming
--   distance is 1 with respect to exactly one correct read in the dataset
--   (or its reverse complement).
--
-- Return a list of all corrections in the form "[old read]->[new read]".
--
-- >> >Rosalind_52
-- >> TCATC
-- >> >Rosalind_44
-- >> TTCAT
-- >> >Rosalind_68
-- >> TCATC
-- >> >Rosalind_28
-- >> TGAAA
-- >> >Rosalind_95
-- >> GAGGA
-- >> >Rosalind_66
-- >> TTTCA
-- >> >Rosalind_33
-- >> ATCAA
-- >> >Rosalind_21
-- >> TTGAT
-- >> >Rosalind_18
-- >> TTTCC
--
-- > TTCAT->TTGAT
-- > GAGGA->GATGA
-- > TTTCC->TTTCA

corr :: [[Dna.Base]] -> Map [Dna.Base] [Dna.Base]
corr xs =
  let counts   = countSequences xs      -- separate 'good' from 'bad'
      goodSeqs = M.filter (> 1) counts
      badSeqs  = M.filter (== 1) counts
  in matchup (M.keys badSeqs) (M.keys goodSeqs)


-- | matches up the 'bad' sequences (xs) with the 'good' (ys) in a map
-- for each x in xs, let z = rev. compl of x
-- * find y in ys where hamming distance of (x,y) == 1
-- * else find y in ys where hamming distance of (z,y) == 1
matchup :: [[Dna.Base]] -> [[Dna.Base]] -> Map [Dna.Base] [Dna.Base]
matchup xs ys = 
  let match seq     = find ((== 1) . hamm seq) ys
      revcMatch seq = revCompl <$> match (revCompl seq)
      findPair seq  = msum [match seq, revcMatch seq]
      dist m seq    = M.insert seq (findPair seq) m
  in M.mapMaybe id (foldl dist M.empty xs)


-- | turns a list of sequences xs into a Map of its count
countSequences :: [[Dna.Base]] -> Map [Dna.Base] Int
countSequences = foldl updMap M.empty


-- | updates map m with bases
updMap :: Map [Dna.Base] Int -> [Dna.Base] -> Map [Dna.Base] Int
updMap m bases =
  let bases'    = revCompl bases
      insRevc   = M.insertWith (+) bases' 1 m  -- insert the rev. compl seq
      insNorm a = M.insertWith (+) bases 1 m   -- insert the unmodified seq
  in maybe insRevc insNorm (M.lookup bases m)


-- | takes a reverse complement of a sequence
revCompl :: [Dna.Base] -> [Dna.Base]
revCompl = reverse . (map Dna.compl)


-- | computes Hamming distance of two lists
hamm :: Eq a => [a] -> [a] -> Int
hamm xs ys = (foldl count 0) $ zip xs ys
  where count acc e = if (fst e /= snd e) then acc + 1 else acc


main :: IO ()
main = do
  inp <- getContents
  let xs    = map (Dna.toBases . F.sequenceData) $ F.fastaLines inp
      corrs = M.toList (corr xs)
      toStr = concat . map show . revCompl
      pprint (a, b) = toStr a ++ "->" ++ toStr b
  mapM_ print (pprint <$> corrs)
