module Rna (
  Base(..),
  Codon(..),
  fromDna,
  toBases,
  toBases',
  toAminoAcids,
  toAminoAcids',
  toCodons,
  codonMap
  ) where

import Common ( readAppend', makeMap, toPair )
import Data.Either ( rights )
import Data.Foldable ( toList )
import Data.Map ( Map )
import qualified Data.Map as M ( lookup )
import qualified Dna
import Protein ( AminoAcid )

data Base
  = A | U | C | G
  deriving (Eq, Show, Read, Ord)


data Codon
  = Triple (Base, Base, Base)
  deriving (Eq, Show, Ord)


-- | transcribes DNA bases to RNA bases
fromDna :: [Dna.Base] -> [Base]
fromDna = map (\b -> if (b == Dna.T) then U else (read. show) b)


-- | creates [Base] from String, ignoring parse failures
toBases :: String -> [Base]
toBases = rights . toBases'


-- | creates [Base] from String
toBases' :: String -> [Either Char Base]
toBases' = toList . (foldl readAppend' mempty)


-- | creates [Codon] from [Base]
toCodons :: [Base] -> [Codon]
toCodons (b1:b2:b3:rest) = Triple (b1,b2,b3) : toCodons rest
toCodons _ = []


-- | creates [AminoAcid] from [Codon], ignoring unmappeds
toAminoAcids :: [Codon] -> [AminoAcid]
toAminoAcids = rights . toAminoAcids'


-- | creates [AminoAcid] from [Codon]
toAminoAcids' :: [Codon] -> [Either Codon AminoAcid]
toAminoAcids' =
  let f acids c = maybe (fl acids c) (fr acids) (M.lookup c codonMap)
      fl acids  = (: acids) . Left
      fr acids  = (: acids) . Right
  in reverse . (foldl f [])


-- | mapping of Codon -> AminoAcid
codonMap :: Map Codon AminoAcid
codonMap = makeMap codonStr toTypedPair


-- | convert to typed values
toTypedPair :: (String, String) -> (Codon, AminoAcid)
toTypedPair (c, p) =
  let toBases   = map (\b -> read [b] :: Base)
      makeCodon = \(w:x:y:z) -> Triple (w,x,y)
  in (makeCodon (toBases c), read p)


-- | copied from rosalind.info
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
