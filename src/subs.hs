-- Given 2 DNA strings s and t return all locations of t as a substring of s
--
-- >>> GATATATGCATATACTT
-- >>> ATAT
-- 2 4 10

subs :: Eq a => [a] -> [a] -> [Bool]
subs [] _ = []
subs _ [] = []
subs s t  =
  let (m,_) = splitAt (length t) s
      n     = subs (tail s) t
  in if (m == t) then (True:n) else (False:n)


main = do
  s <- getLine
  t <- getLine
  let u = subs s t      -- [u1,u2,...]
      v = zip u [1..]   -- [(u1,1), (u2,2), ...]
      w = filter fst v  -- filter for u(n) = True
  print $ map snd w
