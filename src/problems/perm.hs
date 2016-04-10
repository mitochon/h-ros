import           Data.List (intercalate, permutations)

-- | A permutation of length n is an ordering of the positive integers
-- {1,2,…,n} ... for example, π=(5,3,2,1,4) is a permutation of length 5.
--
-- Given a positive integer n, return the total number of permutations of
-- length n, followed by a list of all such permutations (in any order).

main = do
  n <- readLn :: IO Int
  let p = permutations [1..n]
  print $ length p
  mapM_ putStrLn $ map show' p

-- http://stackoverflow.com/questions/19256244/print-list-of-lists-without-brackets
show' :: Show a => [a] -> String
show' = intercalate " " . map show
