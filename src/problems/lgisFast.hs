import           Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as U
import           Data.List (intercalate)


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
-- Note:
-- This is based on the O(n log n) solution summarized in Wikipedia:
-- https://en.wikipedia.org/wiki/Longest_increasing_subsequence
--
-- Translated from C code found in
-- http://www.geeksforgeeks.org/construction-of-longest-monotonically-increasing-subsequence-n-log-n
--
-- Most likely not O(n log n) since Vector (//) operation is O(n)
--
-- References:
-- http://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n
-- http://stackoverflow.com/questions/3992697/longest-increasing-subsequence
-- http://codereview.stackexchange.com/questions/10349/longest-non-decreasing-subsequence-in-haskell


-- | structure to keep track of the left and right index and the key to be id'd in a binary search
data BinarySearchKey a =
  BSK { leftIdx   :: Int
      , rightIdx  :: Int
      , key       :: a }


-- | structure to store intermediate results while calculating the longest increasing subsequence (LIS)
-- orig (O) is the original input array
-- prev (P) is the predecessor array such that P[i] = index of the calculated predecessor of O[i]
-- lookup (L) is the lookup array such that O[L[i]] = the 'tip' of an LIS of length i
-- len is the length of the current longest increasing subsequence
-- See the reference links above for more information
data SequenceMeta a =
  SM { orig   :: Vector a
     , prev   :: Vector a
     , lookup :: Vector a
     , len    :: Int }


-- | performs binary search over the length of 'lookup' on some defined key
-- xs is the original sequence of interest
-- lookup (L) contains indices of 'xs' such that xs[L[0]], xs[L[1]], ..., xs[L[n]] is non-decreasing
findKeyIndex :: Vector Int -> Vector Int -> BinarySearchKey Int -> Int
findKeyIndex xs lookup (BSK l r k)
  | (r - l) <= 1 = r                        -- no more room to split, return the right index
  | otherwise    = let mid = l + (r - l) `div` 2
                       v   = xs ! (lookup ! mid)
                   in if (v >= k)
                         then findKeyIndex xs lookup (BSK l mid k) -- narrow search to left
                         else findKeyIndex xs lookup (BSK mid r k) -- narrow search to right


-- | runs lgis over an input and returns the meta data containing the result
setupLgis :: Vector Int -> SequenceMeta Int
setupLgis xs
  | size < 2  = SM xs seed seed size
  | otherwise = foldl lgis (SM xs seed seed 1) [1 .. (size - 1)]
  where size  = U.length xs
        seed  = U.fromList (take size (repeat 0))


-- | processes meta data for index k, assuming prior information up to (k-1) is incorporated
lgis :: SequenceMeta Int -> Int -> SequenceMeta Int
lgis (SM xs prev lkup len) k
  | currVal < minVal = SM xs prev adjMin len                -- new minimum
  | currVal > maxVal = SM xs addPrevMax addNewMax (len + 1) -- extend LIS w/ new max
  | otherwise        = SM xs addPrev adjKey len             -- replace LIS at keyIdx
  where currVal    = xs ! k
        minVal     = xs ! (lkup ! 0)
        maxVal     = xs ! (lkup ! (len - 1))
        keyIdx     = findKeyIndex xs lkup (BSK 0 (len-1) currVal)
        adjMin     = lkup // [(0, k)]
        adjKey     = lkup // [(keyIdx, k)]
        addNewMax  = lkup // [(len, k)]
        addPrevMax = prev // [(k, lkup ! (len - 1))]
        addPrev    = prev // [(k, lkup ! (keyIdx - 1))]


-- | reconstructs the longest increasing subsequence
reconstruct :: SequenceMeta Int -> [Int]
reconstruct (SM xs prev lkup len) = reconstruct' newS (lkup ! k) []
  where k    = len - 1
        newS = SM xs prev lkup k


-- | inner loop for `reconstruct`
reconstruct' :: SequenceMeta Int -> Int -> [Int] -> [Int]
reconstruct' (SM xs prev lkup len) k r
  | len < 0   = r
  | otherwise = let newV = xs ! k                    -- current value
                    newK = prev ! k                  -- index of predecessor
                    newS = SM xs prev lkup (len - 1) -- backtrack one
                in reconstruct' newS newK (newV:r)


-- copied from perm.hs
show' :: Show a => [a] -> String
show' = intercalate " " . map show


main :: IO ()
main = do
  _      <- getLine
  strSeq <- getLine
  let intSeq  = map (\x -> read x :: Int) $ words strSeq
      ascSeq  = reconstruct (setupLgis (U.fromList intSeq))
      descSeq = reconstruct (setupLgis (U.fromList (reverse intSeq)))

  print "ascending"
  putStrLn $ show' ascSeq

  print "descending"
  putStrLn $ show' (reverse descSeq)
