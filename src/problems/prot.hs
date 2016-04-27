import qualified Protein ( AminoAcid(..) )
import qualified Rna ( toBases, toCodons, toAminoAcids )

-- | Given an RNA string s corresponding to a strand of mRNA, return the protein
-- string encoded by s.
--
-- >>> AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
-- MAMAPRTEINSTRING

prot :: String -> String
prot =
 let aminos = Rna.toAminoAcids . Rna.toCodons . Rna.toBases
     display = concat . map show . filter (/= Protein.Stop)
 in display . aminos

main = interact prot
