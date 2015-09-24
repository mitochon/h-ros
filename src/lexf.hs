import           Data.List (intercalate, permutations)

-- | Given a collection of at most 10 symbols defining an ordered alphabet,
-- and a positive integer n (n≤10), return all strings of length n that
-- can be formed from the alphabet, ordered lexicographically.
--
-- >>> T A G C
-- >>> 2
-- TT
-- TA
-- TG
-- TC
-- AT
-- AA
-- AG
-- AC
-- GT
-- GA
-- GG
-- GC
-- CT
-- CA
-- CG
-- CC

lexf :: [a] -> Int -> [[a]]
lexf gen n
  | n < 1 = []
  | n == 1 = [ x:[] | x <- gen ]
  | otherwise = [ x:xs | x <- gen, xs <- lexf gen (n-1)]


-- modified from perm.hs
show' :: Show a => [a] -> String
show' = filter (/=' ') . filter (/='"') . intercalate " " . map show


main = do
  gen <- getLine
  len <- getLine
  let x = lexf (words gen) (read len :: Int)
  print $ length x
  mapM_ putStrLn $ map show' x


{-

This is a nice two-liner from one of the posted solutions

import Control.Monad
k_mers k alphabet = replicateM k alphabet

-}
