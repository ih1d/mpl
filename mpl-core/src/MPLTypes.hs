module MPLTypes (
    DNA (..),
    RNA (..),
    transcribe,
    countNucleotides,
    reverseComplement,
) where

import Data.Bits (shiftR, (.&.))
import Data.Vector (Vector, (!), reverse)
import Data.Word (Word64)
import Prelude hiding (reverse)

newtype DNA = DNA (Vector Word64, Int) deriving (Eq)
instance Show DNA where
    show (DNA (ws, len)) = showSeq dnaChar ws len

newtype RNA = RNA (Vector Word64, Int) deriving (Eq)
instance Show RNA where
    show (RNA (ws, len)) = showSeq rnaChar ws len

showSeq :: (Word64 -> Char) -> Vector Word64 -> Int -> String
showSeq decode v = go 0
  where
    go _ 0 = []
    go idx remaining =
        let w = v ! idx
            n = min 32 remaining
         in [decode (nucleotideAt w n i) | i <- [0 .. n - 1]] ++ go (idx + 1) (remaining - n)

nucleotideAt :: Word64 -> Int -> Int -> Word64
nucleotideAt w n i = (w `shiftR` (2 * (n - 1 - i))) .&. 3

dnaChar :: Word64 -> Char
dnaChar 0 = 'A'
dnaChar 1 = 'C'
dnaChar 2 = 'G'
dnaChar _ = 'T'

rnaChar :: Word64 -> Char
rnaChar 0 = 'A'
rnaChar 1 = 'C'
rnaChar 2 = 'G'
rnaChar _ = 'U'

transcribe :: DNA -> RNA
transcribe (DNA (vec, len)) = RNA (vec, len)

countNucleotides :: DNA -> (Integer, Integer, Integer, Integer)
countNucleotides (DNA (v, len)) = go 0 len (0, 0, 0, 0)
  where
    go _ 0 acc = acc
    go idx remaining (a, c, g, t) =
        let w = v ! idx
            n = min 32 remaining
            acc' = foldl (count w n) (a, c, g, t) [0 .. n - 1]
         in go (idx + 1) (remaining - n) acc'
    count w n (a, c, g, t) i = case nucleotideAt w n i of
        0 -> (a + 1, c, g, t)
        1 -> (a, c + 1, g, t)
        2 -> (a, c, g + 1, t)
        _ -> (a, c, g, t + 1)

reverseComplement :: DNA -> DNA
reverseComplement (DNA (v, l)) = DNA (reverse v, l)
