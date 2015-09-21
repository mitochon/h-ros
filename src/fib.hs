-- | Given positive integers n and k, find the total number of rabbits R
-- after n months with multiplication factor k
--
-- * The population begins in the first month with a pair of newborn rabbits.
-- * Rabbits reach reproductive age after one month.
-- * In any given month, every rabbit of reproductive age mates with another
--   rabbit of reproductive age.
-- * Exactly one month after two rabbits mate, they produce k pairs of male
--   and female rabbit.
-- * Rabbits never die or stop reproducing.
--
-- Reference http://rosalind.info/problems/fib/
--
-- >>> 5 3
-- 19

fib :: Int -> Int -> Int
fib n k = let (adult, newborn) = fib' n k in adult + newborn


-- | creates (adult, newborn) tuples where
-- adult(n) = adult(n-1) + newborn(n-1)
-- newborn(n) = adult(n-1)

fib' :: Int -> Int -> (Int, Int)
fib' 1 _ = (0,1)
fib' 2 _ = (1,0)
fib' n k =
  let (adult, newborn) = fib' (n-1) k
  in (adult + newborn, adult * k)
