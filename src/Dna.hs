module Dna (
  Base(..),
  toBases,
  toBases',
  compl
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


-- | complement of a DNA base
compl :: Base -> Base
compl b = case b of
            A -> T
            T -> A
            C -> G
            G -> C
