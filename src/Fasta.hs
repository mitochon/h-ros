module Fasta (
  LineId,
  Bases,
  Line,
  fastaLines,
  readAppend
  ) where

import qualified Data.Text.Lazy as L ( lines, null, head, tail )
import           Data.Text.Lazy ( Text )
import           Data.Sequence ( Seq, empty, viewr, ViewR(..), (|>) )
import           Text.Read ( readMaybe )

type LineId = Text
type Bases = Seq Text
type Line = (LineId, Bases)


-- | folds fasta file format into a Sequence of (id, bases)
fastaLines :: Text -> Seq Line
fastaLines = foldLine . L.lines
  where foldLine :: [Text] -> Seq Line
        foldLine = foldl mkLine empty


mkLine :: Seq Line -> Text -> Seq Line
mkLine seq entry =
  if L.null entry then seq else
    let (prevs :> curr@(lineId, bases)) = viewr seq
    in case (L.head entry) of
      '>' -> seq   |> (L.tail entry, empty)
      _   -> prevs |> (lineId      , bases |> entry)


-- | appends a typed 'Read' to the end of a sequence
readAppend :: Read a => Seq a -> Char -> Seq a
readAppend seq c = maybe seq (seq |>) (readMaybe [c])
