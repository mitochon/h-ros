import qualified Data.Map as Map
import qualified Protein  as P

-- | The prefix spectrum of a weighted string is the collection of all its
-- prefix weights. Given a list L of n (n≤100) positive real numbers,
-- return a protein string of length n−1 whose prefix spectrum is equal to L.
--
-- Notes:
-- If multiple solutions exist, pick any.
-- Consult the monoisotopic mass table.
--
-- >>> 3524.8542
-- >>> 3710.9335
-- >>> 3841.974
-- >>> 3970.0326
-- >>> 4057.0646
-- WMQS

-- | looks up a mass weights and returns list of possible proteins
spec :: Foldable t => t Double -> [[P.AminoAcid]]
spec masses =
  let parseWeight   = \acc m -> findMatches m : acc
      findMatches m = Map.foldrWithKey (compareMass m) [] P.massTable
      compareMass m = \k v acc -> if (abs(v-m) < 0.001) then k : acc else acc
  in reverse $ foldl parseWeight [] masses


main = do
  inp <- getContents
  let weights = map (\l -> read l :: Double) $ lines inp
      masses  = zip weights (tail weights)
      prots   = spec $ map (\(a,b) -> abs(a-b)) masses
  if length weights < 2
    then print "not enough elements" >> print weights
    else print (concat prots)
