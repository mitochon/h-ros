-- | Given positive integers n and k where
-- 100 >= n >= 0 and 10 >= k >= 0, return
-- the total number of partial permutations
-- P(n,k) modulo 1,000,000
--
-- >>> 21 7
-- > 51200

pper :: Integral b => b -> b -> b
pper n k = foldl addUp 1 [m..n]
  where m = n - k + 1
        addUp total c = mod (total * c) 1000000


main :: IO ()
main = do
  inp <- getLine
  let (n:k:_) = map (\x -> read x :: Integer) $ words inp
  print $ pper n k
