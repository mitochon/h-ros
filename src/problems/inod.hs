-- | Given a positive integer n, 3 <= n <= 10000, 
-- return the number of internal nodes of any unrooted binary tree having n
-- leaves.
--
-- Note:
-- From https://cs.brown.edu/courses/csci1950-z/asgn/hmwk1_key.pdf 
-- "We can take an unrooted binary tree and transform it into a rooted binary
--  tree by placing a new root node in the middle of any edge, increasing the 
--  number of nodes by 1 and the number of edges by 1. 
--  Note that the root of a rooted binary tree has total degree 2. 
--  In an unrooted binary tree with n leaves, there are n−2 internal nodes..."
-- 
-- If you construct a unrooted binary tree they end up looking like a circle,
-- with the simplest having 3 leaves and one internal node (like >-) and 4
-- leaves having 2 internal node (like >-<)

main :: IO ()
main = do
  inp <- getLine
  print $ (read inp :: Int) - 2
