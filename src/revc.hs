-- | Given a strand takes its reverse complement,
-- where 'A' <-> 'T', 'C' <-> 'G' are complements
--
-- >>> AAAACCCGGT
-- ACCGGGTTTT

revc :: String -> String
revc = map compl . reverse
       where compl c = case c of
               'A' -> 'T'
               'T' -> 'A'
               'C' -> 'G'
               'G' -> 'C'
               _   -> c

main = interact revc
