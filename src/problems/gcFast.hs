import           Control.Monad (liftM2)
import           Data.Ratio

type Id      = String
type Content = (Int,Int)
type Pair    = (Id,Content)

-- | Given DNA strings in fasta format, return the ID of the string having
-- the highest GC content
parseGC :: [String] -> Pair -> Pair -> Pair
parseGC []     q          p = getMax q p
parseGC (x:xs) q@(id,acc) p = case x of
  ('>':nId) -> parseGC xs (nId,(0,0)) (getMax q p)
  nSeq      -> parseGC xs (id, add acc (gcContent nSeq)) p


-- | return the pair with the higher content
getMax :: Pair -> Pair -> Pair
getMax p1 p2 =
  let (_,(n1,d1)) = p1
      (_,(n2,d2)) = p2
      p1' = n1 % d1
      p2' = n2 % d2
  in case (d1,d2) of
    (_,0) -> p1
    (0,_) -> p2
    _     -> if (p1' > p2') then p1 else p2


-- | calculates the GC-content of a DNA string, defined as the number of
-- 'G' and 'C' bases over the length of the sequence
gcContent :: String -> Content
gcContent []    = (0,0)
gcContent seq   = (gcLen,seqLen)
  where gcBases = filter (liftM2 (||) (=='C') (=='G')) seq
        gcLen   = fromIntegral $ length gcBases
        seqLen  = fromIntegral $ length seq


-- | sums the components of Content
add :: Content -> Content -> Content
add (n1,d1) (n2,d2) = (n1+n2, d1+d2)


main = do
  inp <- getContents
  let (id,(n,d)) = parseGC (lines inp) ("",(0,0)) ("",(0,0))
      score = (fromIntegral n) % (fromIntegral d) * 100
  print id
  print $ fromRational score

{-
  Reimplementation of gc.hs that doesn't use '++'
  Ran on MacBook Pro with 2.6Ghz Intel i5 / 16G memory
  Compare statistics below (11 minutes vs 1.5 seconds)
  Input is a 16Mb (reduced) fasta file with 3 regions

%> time runghc gc.hs < b37.fa
"8"
46.823753
runghc gc.hs < b37.fa  688.19s user 7.87s system 99% cpu 11:43.09 total

%> time runghc gcFast.hs < b37.fa        
"8"
46.82375529374118
runghc gcFast.hs < b37.fa  1.43s user 0.12s system 99% cpu 1.566 total

  Update 9/29/15 - after replacing (++) w/ (:)
  gc.hs performs much faster - both are now at par
-}
