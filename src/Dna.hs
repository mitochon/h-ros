module Dna (
  Base(..),
  toBases
  ) where

import           Data.Foldable ( toList )
import qualified Data.Sequence as S ( empty )
import           Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as L ( foldl )
import           Fasta ( readAppend )

data Base
  = A | C | T | G
  deriving (Eq, Show, Read, Ord)


-- | creates [Base] from Text
toBases :: Text -> [Base]
toBases = toList . (L.foldl readAppend S.empty)
