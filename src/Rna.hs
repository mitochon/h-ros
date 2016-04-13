module Rna ( Base(..), Codon(..), codonMap ) where

import Protein
import Data.Map as M hiding (filter, map)


data Base
  = A | U | C | G
  deriving (Eq, Show, Read, Ord)


data Codon
  = Triple (Base, Base, Base)
  deriving (Eq, Show, Ord)


-- | mapping of Codon -> AminoAcid
codonMap :: M.Map Codon AminoAcid
codonMap =
  let toMap = M.fromList . map toTypedPair. toPair . words
  in toMap codonStr


-- | create tuples out of elements of a list
toPair :: [t] -> [(t,t)]
toPair (x:y:xs) = (x,y) : toPair xs
toPair  _       = []


-- | convert to typed values
toTypedPair :: (String, String) -> (Codon, AminoAcid)
toTypedPair (c, p) =
  let toBases   = map (\b -> read [b] :: Base)
      makeCodon = \(w:x:y:z) -> Triple (w,x,y)
  in (makeCodon (toBases c), read p)


codonStr =
  "  UUU F      CUU L      AUU I      GUU V \
   \ UUC F      CUC L      AUC I      GUC V \
   \ UUA L      CUA L      AUA I      GUA V \
   \ UUG L      CUG L      AUG M      GUG V \
   \ UCU S      CCU P      ACU T      GCU A \
   \ UCC S      CCC P      ACC T      GCC A \
   \ UCA S      CCA P      ACA T      GCA A \
   \ UCG S      CCG P      ACG T      GCG A \
   \ UAU Y      CAU H      AAU N      GAU D \
   \ UAC Y      CAC H      AAC N      GAC D \
   \ UAA Stop   CAA Q      AAA K      GAA E \
   \ UAG Stop   CAG Q      AAG K      GAG E \
   \ UGU C      CGU R      AGU S      GGU G \
   \ UGC C      CGC R      AGC S      GGC G \
   \ UGA Stop   CGA R      AGA R      GGA G \
   \ UGG W      CGG R      AGG R      GGG G "
