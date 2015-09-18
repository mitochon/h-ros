type ACTG = (Int,Int,Int,Int)

-- | Count number times each 'base' shows up in a sequence
--
-- >>> countACTG "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
-- (20,12,21,17)

countACTG :: [Char] -> ACTG
countACTG ls = acc ls (0,0,0,0)
  where acc :: [Char] -> ACTG -> ACTG
        acc [] result = result
        acc (x : xs) r@(a,c,t,g) = case x of
          'A' -> acc xs (a+1,c,t,g)
          'C' -> acc xs (a,c+1,t,g)
          'T' -> acc xs (a,c,t+1,g)
          'G' -> acc xs (a,c,t,g+1)
          _   -> acc xs r
