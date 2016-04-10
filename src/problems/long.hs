-- | Given at most 50 DNA strings whose length does not exceed 1 kbp in FASTA
-- format (which represent reads deriving from the same strand of a single
-- linear chromosome), return a shortest superstring containing all the
-- given strings (thus corresponding to a reconstructed chromosome).
--
-- Note:
-- The dataset is guaranteed to satisfy the following condition
-- There exists a unique way to reconstruct the entire chromosome from these
-- reads by gluing together pairs of reads that overlap by more than half their
-- length.
--
-- >>> >Rosalind_56
-- >>> ATTAGACCTG
-- >>> >Rosalind_57
-- >>> CCTGCCGGAA
-- >>> >Rosalind_58
-- >>> AGACCTGCCG
-- >>> >Rosalind_59
-- >>> GCCGGAATAC
--
-- ATTAGACCTGCCGGAATAC
--
-- Breakdown for the above sample:
-- 
-- ATTAGACCTG           -- 56
--    AGACCTGCCG        -- 58
--       CCTGCCGGAA     -- 57
--           GCCGGAATAC -- 59


-- | Assumes a 'perfect' input that contains no unmatched pairs
long' :: [Pair] -> [Pair] -> [Pair] -> [Int] -> String
long' []     []    matched dist = concat $ toSuper matched dist
long' (n:ns) later []      _    = long' ns later [n] []
long' []     later matched dist = long' later [] matched dist
long' (n:ns) later matched dist =
  let max         = maxLen n
      prefixed    = prefixLen' (snd n) (snd (head matched)) 0 max
      suffixed    = prefixLen' (snd (last matched)) (snd n) 0 max
  in case (prefixed, suffixed) of
    (Just a,_)  -> long' ns later (n:matched) (a:dist)
    (_, Just a) -> long' ns later (matched ++ [n]) (dist ++ [a])
    _           -> long' ns (n:later) matched dist


-- | Same as long' but w/ circuit break for inf. loops, 'raw' output
--
-- Some notes:
-- 1st pattern is the terminating condition
-- 3rd pattern is for processing the unmatched pairs
-- The returned value is a triplet of
-- > matched result
-- > distances between the result components
-- > any unmatched leftovers
long :: [Pair] -> [Pair] -> [Pair] -> [Int] -> Int -> ([Pair],[Int],[Pair])
long []     []    matched dist usize = (matched,dist,[])
long (n:ns) later []      _    usize = long ns later [n] [] usize
long []     later matched dist usize =
  if (usize == 0 || (length later) < usize)
  then long later [] matched dist (length later) -- process remaining unmatched
  else (matched,dist,later)                      -- break to avoid inf. loop

long (n:ns) later matched dist usize =
  let max         = maxLen n
      prefixed    = prefixLen' (snd n) (snd (head matched)) 0 max
      suffixed    = prefixLen' (snd (last matched)) (snd n) 0 max
  in case (prefixed, suffixed) of
    (Just a,_)  -> long ns later (n:matched) (a:dist) usize
    (_, Just a) -> long ns later (matched ++ [n]) (dist ++ [a]) usize
    _           -> long ns (n:later) matched dist usize


-- | constructs the superstring from the strands and distances
toSuper pairs dist = zipWith takeN (dist ++ [-1]) (map snd pairs)
  where takeN = \x y -> if (x > 0) then take x y else y


-- | Taking advantage of the fact that for each strand there is another strand
-- that overlap by more than half their length, the maximum length of a matching
-- prefix should be at most 1/2 the length of the strand
maxLen :: Foldable t => (a1, t a) -> Int
maxLen = (`div` 2) . length . snd


-- | return length of prefix p1 where p1+q1=s1 and q1+r2=s2 in that
-- q1 is a common element for both s1 and s2
prefixLen :: (Num a, Ord t) => [t] -> [t] -> a -> Maybe a
prefixLen [] _  _ = Nothing
prefixLen _  [] _ = Nothing
prefixLen s1 s2 n = if mismatch then prefixLen (tail s1) s2 (n+1) else Just n
  where mismatch = any (/= EQ) $ zipWith compare s1 s2


-- | combines prefixLen with maxLen
prefixLen' :: (Num b, Ord b, Ord t) => [t] -> [t] -> b -> b -> Maybe b
prefixLen' s1 s2 n max = prefixLen s1 s2 n >>= checkLen
  where checkLen = \a -> if (a < max) then Just a else Nothing


main = do
  inp <- getContents
  let strands = foldPair (lines inp)
      (s,d,u) = long strands [] [] [] 0
  print $ map fst s
  print $ concat (toSuper s d)


-- copied from splc.hs
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
