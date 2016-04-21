module Rna (
  Base(..),
  Codon(..),
  fromDna,
  toBases,
  toAminoAcids,
  toCodons,
  codonMap  
  ) where

import           Data.Foldable ( toList )
import           Data.Map ( Map )
import qualified Data.Map as M ( lookup, fromList )
import           Data.Sequence ( Seq )
import qualified Data.Sequence as S ( empty )
import           Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as L ( foldl )
import qualified Dna
import           Fasta ( readAppend )
import           Protein ( AminoAcid )

data Base
  = A | U | C | G
  deriving (Eq, Show, Read, Ord)


data Codon
  = Triple (Base, Base, Base)
  deriving (Eq, Show, Ord)


-- | transcribes DNA bases to RNA bases
fromDna :: [Dna.Base] -> [Base]
fromDna = map (\b -> if (b == Dna.T) then U else (read. show) b)


-- | creates [Base] from Text
toBases :: Text -> [Base]
toBases = toList . (L.foldl readAppend S.empty)


-- | creates [Codon] from [Base]
toCodons :: [Base] -> [Codon]
toCodons (b1:b2:b3:rest) = Triple (b1,b2,b3) : toCodons rest
toCodons _ = []


-- | creates [AminoAcid] from [Codon]
toAminoAcids :: [Codon] -> [AminoAcid]
toAminoAcids =
  let f acids c =  maybe acids (: acids) (M.lookup c codonMap)
  in reverse . (foldl f [])


-- | mapping of Codon -> AminoAcid
codonMap :: Map Codon AminoAcid
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
