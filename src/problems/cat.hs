import           Control.Monad (liftM2)
import           Control.Monad.Trans.State (State(..), put, get, runState)
import           Data.List (sortOn)
import qualified Data.Map as M
import qualified Fasta as F
import qualified Rna

-- | Given an RNA string s having
-- 1. the same number of occurrences of 'A' as 'U'
-- 2. the same number of occurrences of 'C' as 'G'
-- 3. the length of the string is at most 300 bp.
--
-- Return the total number of noncrossing perfect matchings of base pair edges in
-- the bonding graph of s modulo 1,000,000.
--
-- >>> > UAGCGUGAUCAC
-- >>> > 2
--
-- Notes
-- 1. For testing, check single complement-paired sequences follows the Catalan
--    number, e.g. "AU" -> 1, "AUAU" -> 2, "AUAUAU" -> 5, "AUAUAUAU" -> 14, etc
-- 2. The initial 'evalChunk' and 'addChunks' are simple, brute-force approach
--    that's too slow to solve the Rosalind data set
-- 3. The final solution involves caching and normalizing the data subsets


-- | Helper structure to figure out where a 'bisect' can be made
data Tally =
  Tally { au :: Int
        , gc :: Int }
  deriving (Eq, Show)


-- | The result of a bisecting a bonding graph into left and right regions
data Chunk =
  Chunk { left  :: [Rna.Base]
        , right :: [Rna.Base]
        , mult  :: Int }
  deriving (Eq, Show)


-- | For storing computation of chunks
type Cache = M.Map [Rna.Base] Int


-- | Updates the tally given some base
mkTally :: Tally -> Rna.Base -> Tally
mkTally (Tally au gc) b = case b of
  Rna.A -> Tally (au + 1) gc
  Rna.U -> Tally (au - 1) gc
  Rna.G -> Tally au (gc + 1)
  Rna.C -> Tally au (gc - 1)


-- | Creates a fresh Tally given some base
newTally :: Rna.Base -> Tally
newTally = mkTally (Tally 0 0)


-- | Bisects a base sequence into Chunks
bisect :: [Rna.Base] -> [Chunk]
bisect [] = []
bisect (x:xs) = bisect' xs [] (newTally x) []


-- | Given some base sequence (z:zs), processes zs with (newTally z)
bisect' :: [Rna.Base] -- ^ xs   - remaining sequences
        -> [Rna.Base] -- ^ ys   - completed sequences, (reverse ys) ++ xs = zs
        -> Tally      -- ^ t    - current tally at this point
        -> [Chunk]    -- ^ done - list of completed chunks
        -> [Chunk]
bisect' xs prior t@(Tally au gc) done =
  let prior'  = reverse $ tail prior     -- exclude the base matching 'z'
      addDone = (Chunk prior' xs 1):done -- is when tally is neutral at (0,0)
  in case (xs, au, gc) of
    ([], 0, 0)   -> addDone
    ([], _, _)   -> done
    (y:ys, 0, 0) -> bisect' ys (y:prior) (newTally y) addDone
    (y:ys, _, _) -> bisect' ys (y:prior) (mkTally t y) done


-- | (*) monoid that recurses on 'addChunks'
evalChunk :: Chunk -> Int
evalChunk (Chunk l r m) = foldl combine m [l, r]
  where combine total bases = if (bases == []) then total
                              else total * addChunks (bisect bases)


-- | 'evalChunk' with caching
evalChunk' :: Chunk -> State Cache Int
evalChunk' (Chunk l r m) = foldl combine (return m) [l, r]
  where combine s bases = s >>= evalBases bases


-- | (+) monoid that recurses on 'evalChunk'
addChunks :: [Chunk] -> Int
addChunks = foldl (\total ch -> mod (total + (evalChunk ch)) 1000000) 0


-- | 'addChunks' with caching
addChunks' :: [Chunk] -> Int -> State Cache Int
addChunks' []     total = return total
addChunks' (x:xs) total = do
  cache <- get
  let (sum, cache') = runState (evalChunk' x) cache
      total'        = (mod (total + sum) 1000000)
  put cache'
  return total' >>= addChunks' xs


-- | Helper for evalChunk'
evalBases :: [Rna.Base] -> Int -> State Cache Int
evalBases []    total = return total
evalBases bases total = do
  cache <- get
  let bases'        = normalize bases
      (sum, cache') = runState (addChunks' (bisect bases) 0) cache
  case M.lookup bases' cache of
    Just c  -> return (total * c)
    Nothing -> put (M.insert bases' sum cache') >> return (total * sum)


-- | Normalizes `xs` to two sets of complement-pairs (p1, p2) where
-- p1 always start with 'A' and p2 always start with 'C' e.g.
--  'AUAU'   == 'UAUA'  == 'GCGC'
--  'GCAUGC' == 'AUCGAU'
normalize :: [Rna.Base] -> [Rna.Base]
normalize xs =
  let zipIndex  = zip [0..] xs
      pick b b' = filter (liftM2 (||)(== b)(== b') . snd)
      auPairs   = pick Rna.A Rna.U zipIndex -- ^ pick only A-U pairs
      cgPairs   = pick Rna.C Rna.G zipIndex -- ^ pick only C-G pairs
      (p1, p2)  = mkOrdered auPairs cgPairs -- ^ p1 has the '0' index
      p1'       = rebase Rna.A Rna.U p1     -- ^ normalize p1 to A <-> U
      p2'       = rebase Rna.C Rna.G p2     -- ^ normalize p2 to C <-> G
  in snd <$> sortOn fst (p1' ++ p2')        -- ^ reassemble, remove index


-- | Given bases b and b' and some xs, where xs is a tupled list of
-- an index and some base z or its complement z', transform z -> b and z' -> b'
-- leaving the index untouched
rebase :: Rna.Base -> Rna.Base -> [(Int, Rna.Base)] -> [(Int, Rna.Base)]
rebase _ _  []       = []
rebase b b' n@(x:xs) = (mkPair (snd x) <$>) <$> n -- ^ apply mkPair to tuple's snd
  where mkPair i j = if (j == i) then b else b'


-- | Reorders z and ys depending on which one contains the '0' index
mkOrdered [] ys       = (ys, [])
mkOrdered z@(x:xs) ys = case x of
  (0, _) -> (z, ys)
  _      -> (ys, z)


main :: IO ()
main = do
  inp <- getContents
  let baseStr = F.sequenceData $ head $ F.fastaLines inp
      chunkSt = addChunks' (bisect (Rna.toBases baseStr)) 0
      (total, cache) = runState chunkSt M.empty
  print total
