import qualified Data.Map as Map

-- | Given an RNA string s corresponding to a strand of mRNA, return the protein
-- string encoded by s.
--
-- >>> AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
-- MAMAPRTEINSTRING

prot :: String -> String
prot [] = []
prot n  =
  let (c,cs) = splitAt 3 n
      p      = Map.lookup c codonMap
  in maybe (prot cs) (++ (prot cs)) p


-- | maps codon -> protein string
codonMap :: Map.Map String String
codonMap = 
  let toPair i   = case i of (x:y:xs) -> (x,y):(toPair xs); _ -> []
      ignoreStop = filter $ (/= "Stop") . snd
      toMap      = Map.fromList . ignoreStop . toPair . words
  in toMap codonStr


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

     
main = interact prot

