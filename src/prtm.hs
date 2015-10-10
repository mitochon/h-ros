import qualified Data.Map as Map

-- | Given a protein string P of length at most 1000 aa,
-- return the total weight of P. Consult the monoisotopic mass table.
-- 
-- >>> SKADYEK
-- 821.392

prtm :: Foldable t => t Char -> Double
prtm s =
  let weight = (\prot -> Map.lookup [prot] massTable)
      calc = (\total protein -> maybe total (+ total) (weight protein))
  in foldl calc 0 s


main = do
  protein <- getLine
  let sum = prtm protein
  print sum


-- copied mostly from prot.hs
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
