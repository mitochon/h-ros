import           Data.Function (on)
import           Data.List     (maximumBy, intercalate)

-- | Given a fasta file return a consensus string and a profile matrix
-- If several possible consensus strings exist, then return any
--
-- >>> >Rosalind_1
-- >>> ATCCAGCT
-- >>> >Rosalind_2
-- >>> GGGCAACT
-- >>> >Rosalind_3
-- >>> ATGGATCT
-- >>> >Rosalind_4
-- >>> AAGCAACC
-- >>> >Rosalind_5
-- >>> TTGGAACT
-- >>> >Rosalind_6
-- >>> ATGCCATT
-- >>> >Rosalind_7
-- >>> ATGGCACT
--
-- ATGCAACT
-- A: 5 1 0 0 5 5 0 0
-- C: 0 0 1 4 2 0 6 1
-- G: 1 1 6 3 0 1 0 0
-- T: 1 5 0 0 0 1 1 6

type ACTG = (Int,Int,Int,Int)

-- | calculates the 'profile' of a set of strings

foldProfile :: [String] -> [ACTG]
foldProfile s =
  let applyFold = foldl append ([],[]) s
      applyProf = (\(a,b) -> prof b ((concat . reverse) a))
      append    = (\(q,r) line@(x:_) -> if (x == '>')
                                        then ([], applyProf (q,r))
                                        else (line : q, r))
  in applyProf applyFold

-- | calculates the 'profile' of one sequence

prof :: [ACTG] -> String -> [ACTG]
prof r      []     = r
prof []     s      = prof [(0,0,0,0)] s
prof (r:rs) (x:xs) =
  let (a,c,t,g) = r
  in case x of
    'A' ->  (a+1,c,t,g) : prof rs xs
    'C' ->  (a,c+1,t,g) : prof rs xs
    'T' ->  (a,c,t+1,g) : prof rs xs
    'G' ->  (a,c,t,g+1) : prof rs xs
    _   ->  r : prof rs xs


-- | returns the consensus given a set of profiles

cons :: [ACTG] -> String
cons r =
  let maxp = (\(a,c,t,g) -> maximumBy (compare `on` snd)
                            [('A',a),('C',c),('T',t),('G',g)])
  in reverse $ foldl (\acc e -> fst (maxp e) : acc) [] r


-- | extracts the corresponding base element from the list

byBase :: [ACTG] -> Char -> [Int]
byBase r b = case b of
  'A' -> map (\(a,_,_,_) -> a) r
  'C' -> map (\(_,c,_,_) -> c) r
  'T' -> map (\(_,_,t,_) -> t) r
  'G' -> map (\(_,_,_,g) -> g) r    
  _   -> []


-- modified from perm.hs
show' :: Show a => [a] -> String
show' = intercalate " " . map show


main = do
  inp <- getContents
  let profile   = foldProfile (lines inp)
      consensus = cons profile
      pprint    = putStrLn . filter (/= '"') . show
  pprint consensus
  pprint ("A: " ++ show' (byBase profile 'A'))
  pprint ("C: " ++ show' (byBase profile 'C'))
  pprint ("G: " ++ show' (byBase profile 'G'))
  pprint ("T: " ++ show' (byBase profile 'T'))
