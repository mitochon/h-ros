-- | Replaces all 'T' with 'U'
--
-- >>> toRNA "GATGGAACTTGACTACGTAAATT"
-- GAUGGAACUUGACUACGUAAAUU

toRNA :: String -> String --[Char] -> [Char]
toRNA = map (\x -> if (x == 'T') then 'U' else x)

main = interact toRNA
