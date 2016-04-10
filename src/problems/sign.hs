import           Control.Monad (replicateM)
import           Data.List     (intercalate, permutations)

-- | A signed permutation of length n is some ordering of the positive integers
-- {1,2,…,n} in which each integer is then provided with either a positive or
-- negative sign (for the sake of simplicity, we omit the positive sign).
-- For example, π=(5,−3,−2,1,4) is a signed permutation of length 5.
--
-- given a positive integer n, return the total number of signed permutations of
-- length n, followed by a list of all such permutations (in any order)
--
-- >>> 2
-- 8
-- -1 -2
-- -1 2
-- 1 -2
-- 1 2
-- -2 -1
-- -2 1
-- 2 -1
-- 2 1

sign :: Int -> [[Int]]
sign n = do
  x <- replicateM n [1,-1]
  y <- permutations [1..n]
  return $ zipWith (*) x y


main = do
  len <- getLine
  let p = sign (read len :: Int)
  print $ length p
  mapM_ putStrLn $ map show' p


-- http://stackoverflow.com/questions/19256244/print-list-of-lists-without-brackets
show' :: Show a => [a] -> String
show' = intercalate " " . map show
