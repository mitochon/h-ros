import qualified Data.Map as Map

-- | Given a DNA string s (of length at most 1 kbp) and a collection of
-- substrings of s acting as introns, return a protein string resulting
-- from transcribing and translating the exons of s.
-- All strings are given in FASTA format.
--
-- Note: Only one solution will exist for the dataset provided.
--
-- >>> >Rosalind_10
-- >>> ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
-- >>> >Rosalind_12
-- >>> ATCGGTCGAA
-- >>> >Rosalind_15
-- >>> ATCGGTCGAGCGTGT
-- MVYIADKQHVASREAYGHMFKVCA

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


-- | cuts all instances of t from s
-- assumes no overlapping instances of t within the cut region (!)
cut :: String -> String -> String
cut [] _       = []
cut s []       = s
cut s@(b:bs) t =
  let l     = length t
      (m,n) = splitAt l s
  in if (m == t) then (cut n t) else b:(cut bs t)


main = do
  inp <- getContents
  let (seq:introns) = map snd $ foldPair (lines inp)
      exons = foldl cut seq introns
  print (prot (toRNA exons))


-- from rna.hs
toRNA :: String -> String --[Char] -> [Char]
toRNA = map (\x -> if (x == 'T') then 'U' else x)


-- from prot.hs
prot :: String -> String
prot [] = []
prot n  =
  let (c,cs) = splitAt 3 n
      p      = Map.lookup c codonMap
  in maybe (prot cs) (++ (prot cs)) p

codonMap :: Map.Map String String
codonMap =
  let toPair i   = case i of (x:y:xs) -> (x,y):(toPair xs); _ -> []
      ignoreStop = filter $ (/= "Stop") . snd
      toMap      = Map.fromList . ignoreStop . toPair . words
  in toMap codonStr

codonStr =
  "  UUU F      CUU L      AUU I      GUU V \
   \ UUC F      CUC L      AUC I      GUC V \
   \ UUA L      CUA L      AUA I      GUA V \
   \ UUG L      CUG L      AUG M      GUG V \
   \ UCU S      CCU P      ACU T      GCU A \
   \ UCC S      CCC P      ACC T      GCC A \
   \ UCA S      CCA P      ACA T      GCA A \
   \ UCG S      CCG P      ACG T      GCG A \
   \ UAU Y      CAU H      AAU N      GAU D \
   \ UAC Y      CAC H      AAC N      GAC D \
   \ UAA Stop   CAA Q      AAA K      GAA E \
   \ UAG Stop   CAG Q      AAG K      GAG E \
   \ UGU C      CGU R      AGU S      GGU G \
   \ UGC C      CGC R      AGC S      GGC G \
   \ UGA Stop   CGA R      AGA R      GGA G \
   \ UGG W      CGG R      AGG R      GGG G "
