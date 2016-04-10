-- | Similar to fib.hs, but with new variable mortality rate m,
-- where after m months the rabbit dies, and k is set to 1,
-- so each pair of rabbit generates exactly 1 pair of rabbit after 1 month.
--
-- Reference http://rosalind.info/problems/fibd/
--
-- >>> 6 3
-- 4

fibd :: Int -> Int -> Int
fibd n k = let (adult, newborn) = fibd' n k in adult + newborn


-- | creates (adult, newborn) tuples where
-- adult(n) = adult(n-1) + newborn(n-1) - deceased(n)
-- newborn(n) = adult(n-1)
-- deceased(n) = newborn(n-m)

fibd' :: Int -> Int -> (Int, Int)
fibd' 1 _  = (0,1)
fibd' 2 _  = (1,0)
fibd' n 1          -- for m == 1 will go between (0,1) to (1,0)
  | even n = (1,0)
  | odd n  = (0,1)
fibd' _ m | m < 1  = (0,0)             
fibd' n m  =
  let (adult, newborn) = fibd' (n-1) m
      deceased = if (n-m < 1) then 0 else snd $ fibd' (n-m) m
  in (adult + newborn - deceased, adult)
