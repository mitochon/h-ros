import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import qualified Data.Sequence       as S
import qualified Data.Vector.Unboxed as U

-- | Given a A DNA string s (of length at most 100 kbp) in FASTA format,
-- return the failure array of s.
--
-- The failure array of s is an array P of length n for which P[k] is the
-- length of the longest substring s[j:k] that is equal to some prefix
-- s[1:k−j+1], where j cannot equal 1 (otherwise, P[k] would always equal k).
-- By convention, P[1]=0.
--
-- The failure array is the pre-compute step in the Knuth-Morris-Pratt algorithm
--
-- >>> >Rosalind_87
-- >>> CAGCATGGTATCACAGCAGAG
-- 0 0 0 1 2 0 0 0 0 0 0 1 2 1 2 3 4 5 3 0 0
--
-- References:
-- http://www.inf.fh-flensburg.de/lang/algorithmen/pattern/kmpen.htm
-- http://tekmarathon.com/2013/05/14/algorithm-to-find-substring-in-a-string-kmp-algorithm/
--
-- Some nice Haskell references for KMP (but does not apply here)
-- from http://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell
-- in detail: http://stackoverflow.com/questions/16694306/knuth-morris-pratt-algorithm-in-haskell


-- | Finds the length of the longest prefix at index i
getLen :: (Eq a, U.Unbox a) => Int -> Int -> U.Vector a -> S.Seq Int -> Int
getLen i j pfx border
  | i == 1                     = 0
  | j == -1                    = j + 1
  | pfx U.! (i-1) == pfx U.! j = j + 1  -- separated w/ (j == -1) for clarity
  | otherwise                  = getLen i (S.index border j) pfx border


-- | Finds the length of the longest prefix for a sequence s
kmp :: (Eq a, U.Unbox a) => [a] -> S.Seq Int
kmp s = foldl findBorders seed s
  where pfx         = U.fromList s                        -- convert to Vector
        seed        = S.singleton (-1)                    -- set border[0] = -1
        getR seq    = let (_ S.:> j) = S.viewr seq in j
        findBorders = (\sq a -> sq S.|> getLen (S.length sq) (getR sq) pfx sq)


main = do
  inp <- getContents
  let (seq:_) = map snd $ foldPair (lines inp)
      prefix  = S.drop 1 (kmp seq)
      noComma = putStrLn . intercalate " " . map show
  noComma $ toList prefix


-- from splc.hs
type Pair = (String,String)

-- | folds fasta file format into [(id,sequence)] string pairs
foldPair :: [String] -> [Pair]
foldPair s = addPair $ foldl addLine ([],[],[]) s
  where addPair = (\(seq,id,acc) ->
                    if (length id > 0)
                    then acc ++ [(id, (concat . reverse) seq)] -- use '++' ok ?!
                    else acc)
        addLine = (\(seq,id,acc) line@(x:nId) ->
                    if (x == '>')
                    then ([], nId, addPair (seq,id,acc))
                    else (line : seq, id, acc))
