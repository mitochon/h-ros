import qualified Data.Map as Map

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
spec :: Foldable t => t Double -> [[String]]
spec masses =
  let parseWeight   = \acc m -> findMatches m : acc
      findMatches m = Map.foldrWithKey (compareMass m) [] massTable
      compareMass m = \k v acc -> if (abs(v-m) < 0.001) then k : acc else acc
  in reverse $ foldl parseWeight [] masses


main = do
  inp <- getContents
  let weights = map (\l -> read l :: Double) $ lines inp
      masses  = zip weights (tail weights)
      prots   = spec $ map (\(a,b) -> abs(a-b)) masses
      takeFst = concat $ map head prots
  if length weights < 2
    then print "not enough elements" >> print weights
    else print takeFst


-- copied from prtm.hs
massTable =
  let toPair i = case i of (x:y:ys) -> (x, read y ::Double):(toPair ys); _ -> []
      toMap = Map.fromList . toPair . words
  in toMap massTableStr


massTableStr =
  "A   71.03711 \
\ C   103.00919 \
\ D   115.02694 \
\ E   129.04259 \
\ F   147.06841 \
\ G   57.02146  \
\ H   137.05891 \
\ I   113.08406 \
\ K   128.09496 \
\ L   113.08406 \
\ M   131.04049 \
\ N   114.04293 \
\ P   97.05276  \
\ Q   128.05858 \
\ R   156.10111 \
\ S   87.03203  \
\ T   101.04768 \
\ V   99.06841  \
\ W   186.07931 \
\ Y   163.06333 "
