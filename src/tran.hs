-- | Given 2 dna strings of equal length, return the transition to
-- transversion ratio
--
-- >>> >Rosalind_0209
-- >>> GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA
-- >>> AGTACGGGCATCAACCCAGTT
-- >>> >Rosalind_2200
-- >>> TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC
-- >>> GGTACGAGTGTTCCTTTGGGT
-- 1.21428571429

data Mutation = Transition | Transversion
              deriving (Eq, Show, Enum)


-- | given 2 strings will return a tuple (Transition,Transversion)
tran :: (Num ti, Num tv) => [Char] -> [Char] -> (ti, tv)
tran a b =
  let f = filter(\t -> fst t /= snd t) $ zip a b
      g = (\(ti,tv) m -> if (m == Transition) then (ti+1,tv) else (ti,tv+1))
  in foldl g (0,0) (map toMutation f)


-- | find the type of mutation
toMutation :: (Char, Char) -> Mutation
toMutation g
  | g `elem` [('A','G'),('G','A'),('C','T'),('T','C')] = Transition
  | otherwise = Transversion


main = do
  inp <- getContents
  let (seq1:seq2:_) = map snd $ foldPair (lines inp)
      (ti,tv) = tran seq1 seq2
      r = if (tv == 0) then 0 else (ti / tv)
  print r


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
