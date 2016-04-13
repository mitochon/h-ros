import Rna
import Protein
import Data.Map as M hiding ( map )
import Data.List as L

-- | Given a protein string of length at most 1000 aa,
-- return the total number of different RNA strings from which
-- the protein could have been translated, modulo 1,000,000.
-- (Don't neglect the importance of the stop codon in protein
-- translation.)
--
-- >>> MA
-- >>> 12
--
-- Insights
-- > every RNA string ends with a stop codon
-- > (8 * 6 * 4) mod 10 == ((8 * 6) mod 10 * 4) mod 10


mrna :: [AminoAcid] -> Int
mrna =
  let addUp total c = mod (total * c) 1000000
      g total acid = maybe total (addUp total) (M.lookup acid acidCountMap)
  in (L.foldl g stopCount)


-- | Lists all AminoAcids in the codon table
acidList :: [AminoAcid]
acidList = map snd $ M.toList Rna.codonMap


-- | Map of AminoAcid -> Counts in the codon table
acidCountMap :: Map AminoAcid Int
acidCountMap =
  let f     = group . sort -- e.g [AA, F, LLL]
      g     = map (\grp -> (head grp, length grp))
      pairs = (g . f) acidList
  in M.fromList pairs


-- | Number of Stop codon in the codon Map
stopCount = maybe 1 id (M.lookup Stop acidCountMap)


main :: IO()
main = do
  aminoAcidStr <- getLine

  let toAcid = map (\b -> read [b] :: AminoAcid)
      total = mrna (toAcid aminoAcidStr)

  print total
