module Dna (
  Base(..),
  toBases,
  toBases'
  ) where

import Common ( readAppend' )
import Data.Either ( rights )
import Data.Foldable ( toList )


data Base
  = A | C | T | G
  deriving (Eq, Show, Read, Ord)


-- | creates [Base] from String, ignoring parse failures
toBases :: String -> [Base]
toBases = rights . toBases'


-- | creates [Base] from String
toBases' :: String -> [Either Char Base]
toBases' = toList . (foldl readAppend' mempty)
