import Data.Set (Set)
import qualified Data.Set as S

-- | Given a positive integer n < 1000 and an adjacency list corresponding to
-- a graph on nn nodes that contains no cycles.
-- Return the minimum number of edges that can be added to the graph to
-- produce a tree.
-- >>> 10
-- >>> 1 2
-- >>> 2 8
-- >>> 4 10
-- >>> 5 9
-- >>> 6 10
-- >>> 7 9
--
-- > 3

type Edge = (Int, Int)

type Bags = (Maybe (Set Int), Maybe (Set Int))


-- | traverses through (x:xs) and checks for Edge (e1,e2).
-- Possible outcomes:
-- A. List is empty
-- B. e1 and e2 membership has been identified
-- If A and B, process accumulated data and produce final result
-- C. Either e1 or e2 membership is unknown
--   1. Both e1, e2 are on the same set
--      If input data is valid, this should NEVER be the case
--      else return as is because all sets are up-to-date
--   2. e1 is a member of x. Move x into a bag for further observation
--   3. e2 is a member of x. Move x into a bag for further observation
--   4. Neither is found. Recurse

tree :: [Set Int] -> Bags -> [Set Int] -> Edge -> [Set Int]
tree done b                  []     e         = addBag done b e
tree done b@(Just s, Just t) xs     e         = addBag (xs ++ done) b e
tree done b@(b1,b2)          (x:xs) e@(e1,e2) =
  let i1 = S.lookupIndex e1 x
      i2 = S.lookupIndex e2 x
  in case (i1,i2) of
    (Just s, Just t) -> (x:xs) ++ done
    (Just s, _)      -> tree done (Just x, b2) xs e
    (_, Just s)      -> tree done (b1, Just x) xs e
    _                -> tree (x:done) b xs e


-- | processes accumulated information to produce a new [Set Int]
addBag :: [Set Int] -> Bags -> Edge -> [Set Int]
addBag done (b1,b2) (e1,e2) = case (b1,b2) of
    (Just s, Just t) -> (S.union s t) : done                  -- merge the 2 sets
    (Just s, _)      -> (S.insert e2 s) : done                -- add e2 to the set
    (_, Just s)      -> (S.insert e1 s) : done                -- add e1 to the set
    _                -> (S.insert e1 (S.singleton e2)) : done -- create a new set


-- | constructs an Edge from a String
mkEdge :: String -> Edge
mkEdge s =
  let (x:y:zs) = map (\x -> read x :: Int) $ words s
  in (x,y)


main :: IO ()
main = do
  inp <- getContents
  let (x:xs)   = lines inp
      numNodes = read x :: Int
      edges    = mkEdge <$> xs
      trees    = foldl (tree [] (Nothing, Nothing)) [] edges
      inTrees  = foldl S.union S.empty trees -- set of all numbers in the trees
      all      = S.fromList [1..numNodes]    -- all numbers 1 to numNodes
      missing  = all S.\\ inTrees            -- calculate missing
      minEdges = (length trees) - 1 + (S.size missing)

  putStrLn "trees:"
  mapM_ putStrLn $ map show trees

  putStrLn "missing:"
  putStrLn $ show missing

  putStrLn "min edges:"
  print minEdges

-- | Follow up note: nice solution from @1HaskellADay
-- http://lpaste.net/1052721051462533120 where
--
-- minEdgesToTreeGraph s = (read.head.lines) s - (length.lines) s
