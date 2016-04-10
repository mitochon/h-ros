-- | Given two strings s and t of equal length, the Hamming distance between s and t,
-- denoted dH(s,t), is the number of corresponding symbols that differ in s and t
--
-- >>> GAGCCTACTAACGGGAT
-- >>> CATCGTAATGACGGCCT
-- 7
hamm :: String -> String -> Int
hamm a b = length . filter(\t -> fst t /= snd t) $ zip a b

main = do
  a <- getLine
  b <- getLine
  print $ hamm a b
