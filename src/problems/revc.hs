import Dna ( compl )

-- | Given a strand takes its reverse complement,
-- where 'A' <-> 'T', 'C' <-> 'G' are complements
--
-- >>> AAAACCCGGT
-- ACCGGGTTTT

revc :: String -> String
revc = show . map Dna.compl . read . reverse

main = interact revc
