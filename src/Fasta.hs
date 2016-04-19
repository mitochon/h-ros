module Fasta (
  LineId,
  Bases,
  Line,
  fastaLines
  ) where

import qualified Data.Text.Lazy as L
import Data.Sequence (Seq, empty, viewr, ViewR(..), (|>))

type LineId = L.Text
type Bases = Seq L.Text
type Line = (LineId, Bases)


-- | folds fasta file format into a Sequence of (id, bases)
fastaLines :: L.Text -> Seq Line
fastaLines = foldLine . L.lines
  where foldLine :: [L.Text] -> Seq Line
        foldLine = foldl mkLine empty


mkLine :: Seq Line -> L.Text -> Seq Line
mkLine seq entry =
  if L.null entry then seq else
    let (prevs :> curr@(lineId, bases)) = viewr seq
    in case (L.head entry) of
      '>' -> seq   |> (L.tail entry, empty)
      _   -> prevs |> (lineId      , bases |> entry)
