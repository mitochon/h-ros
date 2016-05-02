import           Control.Monad (forM_)
import           Data.Array
import qualified Fasta as F

-- | Given a collection of DNA strings in FASTA format having total length at
-- most 10 kbp, return the adjacency list corresponding to O3, in any order.
-- You may return edges in any order.
--
-- >>> >Rosalind_0498
-- >>> AAATAAA
-- >>> >Rosalind_2391
-- >>> AAATTTT
-- >>> >Rosalind_2323
-- >>> TTTTCCC
-- >>> >Rosalind_0442
-- >>> AAATCCC
-- >>> >Rosalind_5013
-- >>> GGGTGGG
--
-- Rosalind_0498 Rosalind_2391
-- Rosalind_0498 Rosalind_0442
-- Rosalind_2391 Rosalind_2323
--
-- Insights:
-- Only need to take the first and last 3 letters of any sequence
--

data Point a = Prefix a | Suffix a  deriving (Eq,Show)

type Node a = (String, Point a, Point a)


grph nodes =
  let len    = length nodes
      i      = listArray (1, len) nodes                     -- create node array
      m      = [ (x,y) | x <- [1..len], y <- [1..len] ]     -- n x n matrix
      isEdge = \(x,y) -> (x /= y) && isMatch (i ! x) (i ! y)
      add    = \(x,y) -> getEdge (i ! x) (i ! y)
  in foldl (\acc e -> if (isEdge e) then (add e):acc else acc) [] m


-- | returns a tuple (id1,id2)
getEdge :: Node a -> Node a -> (String,String)
getEdge (id1,_,_) (id2,_,_) = (id1,id2)


-- | checks if the suffix of first node == prefix of second node
isMatch :: Eq a => Node a -> Node a -> Bool
isMatch (_, _,Suffix s) (_,Prefix p, _) = s == p


-- | turns a Pair into Node by taking the first and last n elements
fromPair :: Int -> F.Pair -> Node String
fromPair n (F.Pair id seq) = (id, pfx, sfx)
  where pfx = Prefix $ take n seq                        -- first n chars
        sfx = Suffix $ reverse (take n (reverse seq))    -- last n chars


main = do
  inp <- getContents
  let nodes = map (fromPair 3) $ F.fastaLines inp
  forM_ (grph nodes) $ \(a,b) ->
    putStr a >> putStr " " >> putStrLn b
