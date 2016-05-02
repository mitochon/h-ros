import           Data.List
import qualified Data.Map   as Map
import           Data.Tuple (swap)
import qualified Protein    as P

-- | Given a list L containing 2n+3 positive real numbers (n≤100):
-- The first number in L is the parent mass of a peptide P, and all other
-- numbers represent the masses of some b-ions and y-ions of P
-- (in no particular order).
-- Return a protein string t of length n for which there exist two positive
-- real numbers w1 and w2 such that for every prefix p and suffix s of t,
-- each of w(p)+w1 and w(s)+w2 is equal to an element of L.
-- (In other words, there exists a protein string whose t-prefix and
-- t-suffix weights correspond to the non-parent mass values of L.)
-- If multiple solutions exist, you may output any one.
--
-- >>> 1988.21104821
-- >>> 610.391039105
-- >>> 738.485999105
-- >>> 766.492149105
-- >>> 863.544909105
-- >>> 867.528589105
-- >>> 992.587499105
-- >>> 995.623549105
-- >>> 1120.6824591
-- >>> 1124.6661391
-- >>> 1221.7188991
-- >>> 1249.7250491
-- >>> 1377.8200091
-- KEKEP
--
-- Insights
-- > list of masses will be unique (no duplicates)
-- > numbers can be paired so their sum is constant, e.g.
--   similar to 1..10 where 1+10 = 2+9 = 3+7 = ...
-- > going from x_n to x_n+1 the delta has to match a protein mass
-- > if there is no match then the pair belongs to the other end

full :: [(Double, Double)] -> [(Double, Double)] -> [[P.AminoAcid]] -> [[P.AminoAcid]]
full []         _  _  = []
full (x1:[])    [] ps = reverse ps                    -- terminating condition
full (x1:[])    qs ps = full (x1:(map swap qs)) [] ps -- process remaining qs
full (x1:x2:xs) qs ps =
  let p = findMatches (fst x2 - fst x1)
  in if (length p > 0)
     then full (x2:xs) qs (p:ps)                      -- add p to the front
     else full (x1:xs) (x2:qs) ps                     -- add unmatched x2 to qs


-- | find possible protein matches for a given weight
findMatches :: Double -> [P.AminoAcid]
findMatches m = Map.foldrWithKey compareMass [] P.massTable
  where compareMass = \k v acc -> if (abs(v-m) < 0.001) then k : acc else acc


-- | takes an unsorted list into sorted, validated ion pairs
toIonPairs :: (Fractional a, Ord a) => [a] -> a -> Maybe [(a, a)]
toIonPairs i t =
  let len        = (length i `div` 2)
      (fst, snd) = splitAt len (sort i)
      result     = zip fst (reverse snd)          -- (a1,a_n),(a2,a_n-1),..
      sameLength = any (\(a,b) -> t-a-b < 0.001)  -- check t = a + b
      isValid    = (length i `mod` 2 == 0) && sameLength result
  in if isValid then Just result else Nothing


main = do
  inp <- getContents
  let (total:ionMass) = map (\l -> read l :: Double) $ lines inp
      pairs           = toIonPairs ionMass total
  case pairs of
    Nothing -> print "invalid input"
    Just a -> let proteinstr = full a [] []
              in print $ concat proteinstr
