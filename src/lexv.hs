import           Data.List (intercalate, sortBy)

-- | Given a permutation of at most 12 symbols defining an ordered alphabet 𝒜
-- and a positive integer n (n≤4), return all strings of length at most n
-- formed from 𝒜, ordered lexicographically
--
-- >>> D N A
-- >>> 3
-- D
-- DD
-- DDD
-- DDN
-- DDA
-- DN
-- DND
-- DNN
-- DNA
-- DA
-- DAD
-- DAN
-- DAA
-- N
-- ND
-- NDD
-- NDN
-- NDA
-- NN
-- NND
-- NNN
-- NNA
-- NA
-- NAD
-- NAN
-- NAA
-- A
-- AD
-- ADD
-- ADN
-- ADA
-- AN
-- AND
-- ANN
-- ANA
-- AA
-- AAD
-- AAN
-- AAA

lexv :: Eq a => [a] -> Int -> [[a]]
lexv gen n =
  let gen'   = concat $ map (lexf gen) [1..n]
      lexmap = zip gen ['a'..]
  in sortBy (lexcomp lexmap) gen'


-- | compare 2 lexical strings using a lookup map

lexcomp :: (Eq a, Show b) => [(a, b)] -> [a] -> [a] -> Ordering
lexcomp m s1 s2 =
  let a = transpose s1 m
      b = transpose s2 m
  in compare a b


-- | re-maps a lexical string, e.g in lexical {D,N,A} - "DD" -> "11"

transpose :: (Eq a, Show b) => [a] -> [(a,b)] -> String
transpose []     _ = []
transpose (k:ks) m =
  let v = lookup k m
      w = transpose ks m
  in maybe w ((++ w) . show) v


-- copied below from lexf.hs
lexf :: [a] -> Int -> [[a]]
lexf gen n
  | n < 1 = []
  | n == 1 = [ x:[] | x <- gen ]
  | otherwise = [ x:xs | x <- gen, xs <- lexf gen (n-1)]


show' :: Show a => [a] -> String
show' = filter (/=' ') . filter (/='"') . intercalate " " . map show


main = do
  gen <- getLine
  len <- getLine
  let x = lexv (words gen) (read len :: Int)
  mapM_ putStrLn $ map show' x
