import           Data.List (intercalate)

-- | For a fixed positive integer k, order all possible k-mers taken
-- from an underlying alphabet lexicographically.
--
-- Given a DNA string s in FASTA format return the 4-mer composition
-- of s.
--
-- >Rosalind_6431
-- CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGG
-- CCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGT
-- TTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCA
-- AATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCG
-- GGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGA
-- CTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTA
-- CCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG
--
-- 4 1 4 3 0 1 1 5 1 3 1 2 2 1 2 0 1 1 3 1 2 1 3 1 1 1 1 2 2 5 1
-- 3 0 2 2 1 1 1 1 3 1 0 0 1 5 5 1 5 0 2 0 2 1 2 1 1 1 2 0 1 0 0
-- 1 1 3 2 1 0 3 2 3 0 0 2 0 8 0 0 1 0 2 1 3 0 0 0 1 4 3 2 1 1 3
-- 1 2 1 3 1 2 1 2 1 1 1 2 3 2 1 1 0 1 1 3 2 1 2 6 2 1 1 1 2 3 3
-- 3 2 3 0 3 2 1 1 0 0 1 4 3 0 1 5 0 2 0 1 2 1 3 0 1 2 2 1 1 0 3
-- 0 0 4 5 0 3 0 2 1 1 3 0 3 2 2 1 1 0 2 1 0 2 2 1 2 0 2 2 5 2 2
-- 1 1 2 1 2 2 2 2 1 1 3 4 0 2 1 1 0 1 2 2 1 1 1 5 2 0 3 2 1 1 2
-- 2 3 0 3 0 1 3 1 2 3 0 2 1 2 2 1 2 3 0 1 2 3 1 1 3 1 0 1 1 3 0
-- 2 1 2 2 0 2 1 1

-- | find kmer content of a sequence
kmer :: Eq a => [a] -> [[a]] -> [Int]
kmer seq kmers =
  let countK = length . filter id . subs seq
  in map countK kmers


main = do
  inp <- getContents
  let (seq:_) = foldPair $ words inp
      indices = kmer (snd seq) (lexf "ACGT" 4) -- assume 'A C G T' lexc
  print $ show' indices


-- from sign.hs
show' :: Show a => [a] -> String
show' = intercalate " " . map show


-- from subs.hs
subs :: Eq a => [a] -> [a] -> [Bool]
subs [] _ = []
subs _ [] = []
subs s t  =
  let (m,_) = splitAt (length t) s
      n     = subs (tail s) t
  in if (m == t) then (True:n) else (False:n)


-- from splc.hs
type Pair = (String,String)

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


-- from lexf.hs
lexf :: [a] -> Int -> [[a]]
lexf gen n
  | n < 1 = []
  | n == 1 = [ x:[] | x <- gen ]
  | otherwise = [ x:xs | x <- gen, xs <- lexf gen (n-1)]
