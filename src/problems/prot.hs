import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LI
import qualified Protein ( AminoAcid(..) )
import qualified Rna ( toBases, toCodons, toAminoAcids )

-- | Given an RNA string s corresponding to a strand of mRNA, return the protein
-- string encoded by s.
--
-- >>> AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
-- MAMAPRTEINSTRING

prot :: Text -> Text
prot = 
 let aminos = Rna.toAminoAcids . Rna.toCodons . Rna.toBases
     display = L.concat . map (L.pack . show) . filter (/= Protein.Stop)
 in display . aminos

main = LI.interact prot
