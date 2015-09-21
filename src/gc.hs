import           Control.Monad (liftM2)

-- | Given DNA strings in fasta format, return the ID of the string having
-- the highest GC content
parseGC :: [String] -> (String,String) -> (String,Float) -> (String,Float)
parseGC []     q          p = getMax q p
parseGC (x:xs) q@(id,seq) p = case x of
  ('>':nId) -> parseGC xs (nId, "") (getMax q p)
  nSeq      -> parseGC xs (id, seq ++ nSeq) p


-- | evaluates the first tuple and compares it against the second tuple
-- and return the tuple with the maximum associated value
getMax :: (String,String) -> (String,Float) -> (String,Float)
getMax (id,seq) p = let p'  = (id, gcContent seq)
                        max = if (snd p' > snd p) then p' else p
                    in max


-- | calculates the GC-content of a DNA string, defined as the number of
-- 'G' and 'C' bases over the length of the sequence
gcContent :: String -> Float
gcContect []    = 0
gcContent seq   = gcLen / seqLen * 100
  where gcBases = filter (liftM2 (||) (=='C') (=='G')) seq
        gcLen   = fromIntegral $ length gcBases
        seqLen  = fromIntegral $ length seq

main = do
  inp <- getContents
  let (id, score) = parseGC (lines inp) ("","") ("",0)
  print id
  print score
