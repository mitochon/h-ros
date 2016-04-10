{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens               ((^.))
import           Control.Monad              (forM)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List                  (intercalate)
import           Network.Wreq
import           Text.Printf                (printf)

-- | Given at most 15 UniProt Protein Database access IDs,
-- then for each protein possessing the N-glycosylation motif, return
-- output its given access ID followed by a list of locations in the protein
-- string where the motif can be found.
--
-- Notes:
-- 1) lookup protein information
--    http://www.uniprot.org/uniprot/{uniprot_id}.fasta
--    e.g. http://www.uniprot.org/uniprot/B5ZC00.fasta
-- 2) a protein motif is represented by a shorthand as follows:
--    [XY] means "either X or Y" and
--    {X} means "any amino acid except X."
--    For example, the N-glycosylation motif is written as N{P}[ST]{P}.
--
-- >>> A2Z669
-- >>> B5ZC00
-- >>> P07204_TRBM_HUMAN
-- >>> P20840_SAG1_YEAST
-- B5ZC00
-- 85 118 142 306 395
-- P07204_TRBM_HUMAN
-- 47 115 116 382 409
-- P20840_SAG1_YEAST
-- 79 109 135 248 306 348 364 402 485 501 614


data Motif a = Exactly a | Except a | Any [a] deriving (Show)


-- | defined as N{P}[ST]{P}
nGlycosylationMotif = [Exactly 'N', Except 'P', Any ['S','T'], Except 'P']


-- | check if the motif matches starting at the beginning of the sequence
matchMotif :: Eq a => [Motif a] -> [a] -> Bool
matchMotif [] _ = True
matchMotif _ [] = False
matchMotif (m:ms) (x:xs) = case m of
  Exactly e -> if (e == x) then (matchMotif ms xs) else False
  Except e  -> if (e /= x) then (matchMotif ms xs) else False
  Any e     -> if (x `elem` e) then (matchMotif ms xs) else False


-- | scans motif for an entire sequence
scanMotif :: Eq a => [Motif a] -> [a] -> [Bool]
scanMotif m []       = []
scanMotif m n@(x:xs) = matchMotif m n : scanMotif m xs


-- | finds the location(s) of a motif in a sequence
indexMotif :: (Enum a1, Eq a, Num a1) => [Motif a] -> [a] -> [a1]
indexMotif m n =
  let tuples = zip [1..] (scanMotif m n)
  in map fst (filter snd tuples)

-- | looks up uniprot website for a given protein
fetchProtein :: String -> IO (Response C8.ByteString)
fetchProtein p = get (printf "http://www.uniprot.org/uniprot/%s.fasta" p)


-- | assuming fasta format, drops the first line and concat the rest as String
parseSeq :: C8.ByteString -> [Char]
parseSeq = C8.unpack . C8.concat . drop 1 . C8.lines


main = do
  inp <- getContents
  let pNames = lines inp
  pSeqs   <- forM pNames $ \p -> do
    pData <- fetchProtein p
    if (pData ^. responseStatus . statusCode == 200)
      then return (pData ^. responseBody)
      else return C8.empty
  let indices     = map (indexMotif nGlycosylationMotif . parseSeq) pSeqs
      hasMotif    = filter (\x -> length(snd x) > 0) $ zip pNames indices
      noComma     = putStrLn . intercalate " " . map show
      noQuotes    = putStrLn . filter (/= '"') . show
      prettyprint = (\(a,b) -> noQuotes a >> noComma b)
  mapM_ prettyprint hasMotif

