module Fasta (
  Pair(..),
  fastaLines
  ) where

import Data.Foldable ( toList )
import Data.Sequence ( Seq, empty, viewr, ViewR(..), (|>) )


data Pair =
  Pair { header :: String
       , sequenceData :: String }
  deriving (Eq , Show)


-- | folds fasta file format into [Pair]
fastaLines :: String -> [Pair]
fastaLines = toPair . toList . foldLine . lines
  where
    foldLine :: [String] -> Seq (String, Seq String)
    foldLine = foldl mkLine empty

    toPair :: [(String, Seq String)] -> [Pair]
    toPair = map (\(h, s) -> Pair h ((concat . toList) s))


-- | folds an individual line
mkLine :: Seq (String, Seq String) -> String -> Seq (String, Seq String)
mkLine seq entry =
  let (prevs :> (h, s)) = viewr seq
  in case (take 1 entry) of
    []  -> seq
    ">" -> seq   |> (tail entry, empty) -- new header, empty body
    _   -> prevs |> (h, s |> entry)     -- keep header, append body
