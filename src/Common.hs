module Common (
  readAppend,
  readAppend',
  makeMap,
  toPair
  ) where

import Data.Sequence ( Seq, (|>) )
import Data.Map ( Map, fromList )
import Text.Read ( readMaybe )


-- | appends `a` to the end of a sequence, else no change if the parse fail
readAppend :: Read a => Seq a -> Char -> Seq a
readAppend seq c = maybe seq (seq |>) (readMaybe [c])


-- | appends `a` to the end of a sequence
readAppend' :: Read a => Seq (Either Char a) -> Char -> Seq (Either Char a)
readAppend' seq c = maybe (fl c) fr (readMaybe [c])
  where fl = (seq |>) . Left
        fr = (seq |>) . Right


-- | mapping of a -> b
makeMap :: Ord a => String -> ((String, String) -> (a, b)) -> Map a b
makeMap str toTypedPair =
  let toMap = fromList . map toTypedPair. toPair . words
  in toMap str


-- | create tuples out of elements of a list
toPair :: [t] -> [(t,t)]
toPair (x:y:xs) = (x,y) : toPair xs
toPair  _       = []
