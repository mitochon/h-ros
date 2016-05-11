import Data.Maybe
import Data.List

-- | A subsequence of a permutation is a collection of elements of the
-- permutation in the order that they appear.
--
-- For example, (5, 3, 4) is a subsequence of (5, 1, 3, 4, 2).
--
-- A subsequence is increasing if the elements of the subsequence increase,
-- and decreasing if the elements decrease.
--
-- For example, given the permutation (8, 2, 1, 6, 5, 7, 4, 3, 9),
-- an increasing subsequence is (2, 6, 7, 9), and
-- a decreasing subsequence is (8, 6, 5, 4, 3).
--
-- Given a positive n <= 10000 followed by a permutation of length n
-- Return a longest increasing subsequence of n
-- followed by a longest decreasing subsequence of n.
--
-- >>> 5
-- >>> 5 1 4 2 3
-- 1 2 3
-- 5 4 2
--
-- Note: this is a brute force solution that will time out (> 5 min)
-- when solving a rosalind dataset (array of size ~10k)


-- | helper structure to deliniate things that are done vs in progress
data InProgress a =
  InProgress { done    :: [a]
             , notDone :: [a] }
  deriving Show


-- | for capturing intermediate results
type Pipeline a = ([[a]], [InProgress a])


lgis :: Ord a => [a] -> (a -> a -> Bool) -> [[a]]
lgis xs ord =
  let segments = (putInProgress . (rmUnordered ord)) <$> tails xs
      answer   = resolve ord 0 [] $ catMaybes segments
  in reverse <$> answer


-- | resolves all elements that are InProgress given some ordering
resolve :: Ord a => (a -> a -> Bool) -> Int -> [[a]] -> [InProgress a] -> [[a]]
resolve ord l done [] = done
resolve ord l done (x:xs) =
  let (d, p) = resolveOne ord x
      (m, n) = foldl pickLongest (l, done) d
  in resolve ord m n (p ++ xs)


-- | given (currentLength, xs) compares the length against ys
pickLongest :: (Int, [[a]]) -> [a] -> (Int, [[a]])
pickLongest (l, xs) ys = let m = length ys in case compare l m of
  EQ -> (l, ys:xs)
  GT -> (l, xs)
  LT -> (m, [ys])


-- | puts a list (x:xs) into InProgress [x] xs
putInProgress [] = Nothing
putInProgress (x:xs) = Just $ InProgress [x] xs


-- | removes elements in the list that are not (>) or (<) than the previous
-- e.g.  (>) -> [3,2,5,8,7] -> [3,5,8.7]
rmUnordered :: Ord t => (t -> t -> Bool) -> [t] -> [t]
rmUnordered ord [] = []
rmUnordered ord (x:xs) = let y = filter (flip ord x) xs in x:y


-- | resolves one InProgress element into a tuple of (done, stillInProgress)
resolveOne :: Ord a => (a -> a -> Bool) -> InProgress a -> Pipeline a
resolveOne ord (InProgress d []) = ([d],[])
resolveOne ord (InProgress d n)  =
  let expand = reverse <$> foldl (forkOrAppend ord) [] n
   in foldl (moveOne d) ([],[]) expand


-- | moves one item into the pipeline
moveOne :: [a] -> Pipeline a -> [a] -> Pipeline a
moveOne d (done, remaining) ps =
  case ps of
    (x:[]) -> ((x:d):done, remaining)
    (x:xs) -> (done, (InProgress (x:d) xs): remaining)
    _      -> (done, remaining)


-- | given a new element n, append to elms in xs that matches
-- the test, else if no matches, fork as a new element
-- e.g.
-- (>) -> [[5],[4],[2]] -> 3 -> [[3,2],[5],[4]] (append)
-- (>) -> [[5],[4]] -> 3     -> [[5],[4],[3]]   (fork)
forkOrAppend :: Ord a => (a -> a -> Bool) -> [[a]] -> a -> [[a]]
forkOrAppend ord xs n =
  case partition ((ord n) . last) xs of -- 'last' is O(n), ouch
    ([], _ ) -> [n] : xs
    (ys, zs) -> map (n:) ys ++ zs


-- copied from perm.hs
show' :: Show a => [a] -> String
show' = intercalate " " . map show


main :: IO ()
main = do
  _      <- getLine
  strSeq <- getLine
  let intSeq  = map (\x -> read x :: Int) $ words strSeq
      ascSeq  = lgis intSeq (>)
      descSeq = lgis intSeq (<)
  print "ascending"
  mapM_ putStrLn $ map show' ascSeq
  print "descending"
  mapM_ putStrLn $ map show' descSeq
