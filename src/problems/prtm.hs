import qualified Data.Map as Map
import qualified Protein as P

-- | Given a protein string P of length at most 1000 aa,
-- return the total weight of P. Consult the monoisotopic mass table.
--
-- >>> SKADYEK
-- 821.392

prtm :: Foldable t => t Char -> Double
prtm s =
  let weight = (\prot -> Map.lookup (read [prot]) P.massTable)
      calc = (\total protein -> maybe total (+ total) (weight protein))
  in foldl calc 0 s


main = do
  protein <- getLine
  print $ prtm protein
