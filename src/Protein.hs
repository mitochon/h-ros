module Protein where

data AminoAcid
  = A | C | D | E | F | G | H
  | I | K | L | M | N | P | Q
  | R | S | T | V | W | Y | Stop
  deriving (Eq, Show, Read, Ord)
