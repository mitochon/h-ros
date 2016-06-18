import           Control.Monad (liftM2)
import qualified Dna

-- | Given:
-- * a positive integer N <= 100000
-- * a number x between 0 and 1 that is the GC-content
-- * and a DNA string s of length at most 10 bp.
--
-- Return the probability that if N random DNA strings having the same
-- length as s are constructed with GC-content x then at least one of
-- the strings equals s.
-- We allow for the same random string to be created more than once.
--
-- >> 90000 0.6
-- >> ATAGCCGA
--
-- > 0.689
-- 
-- Notes
--  for string s = [S1, S2, .., SN], the
--  P(random str == s | GCx) = p1 * p2 * ... * pN = q
--  p1 = P(base|GCx), in the above example p1 = P('A'|GC=.6) = (1-.6)/2
--  P(s doesn't happen) = r^N
--  r = 1 - q = 1 - ((1-x)/2)^(#A+#C) * (x/2)^(#G+#C)
--  P(s happens >= 1) = 1 - P(s doesn't happen) = 1 - r

rstr :: Int -> Float -> [Dna.Base] -> Float
rstr n gc xs =
  let count b b' = length . filter (liftM2 (||)(== b)(== b'))
      atCount = count Dna.A Dna.T xs
      gcCount = count Dna.G Dna.C xs
      q       = ((1-gc)/2)^atCount * (gc/2)^gcCount
      r       = (1-q)^n
  in (1-r)


main :: IO ()
main = do
  s <- getLine
  t <- getLine
  let (n : gc : _) = words s
  print $ rstr (read n :: Int) (read gc :: Float) (Dna.toBases t)
