module Common (
  readAppend,
  readAppend'
  ) where

import Data.Sequence ( Seq, (|>) )
import Text.Read ( readMaybe )


-- | appends `a` to the end of a sequence, else no change if the parse fail
readAppend :: Read a => Seq a -> Char -> Seq a
readAppend seq c = maybe seq (seq |>) (readMaybe [c])


-- | appends `a` to the end of a sequence
readAppend' :: Read a => Seq (Either Char a) -> Char -> Seq (Either Char a)
readAppend' seq c = maybe (fl c) fr (readMaybe [c])
  where fl = (seq |>) . Left
        fr = (seq |>) . Right
