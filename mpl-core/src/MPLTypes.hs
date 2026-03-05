module MPLTypes (
    DNA (..),
    RNA (..),
    transcribe,
    countNucleotides,
    reverseComplement,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Vector (Vector, fromList, (!))
import Data.Word (Word64)

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

nucleotideAt :: Word64 -> Int -> Int -> Word64
nucleotideAt w n i = (w `shiftR` (2 * (n - 1 - i))) .&. 3

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
reverseComplement (DNA (_, 0)) = DNA (fromList [], 0)
reverseComplement (DNA (v, len)) = DNA (fromList (packWords revComp), len)
  where
    numWords = (len + 31) `div` 32
    allNucs = concatMap extractWord [0 .. numWords - 1]
    extractWord idx =
        let remaining = len - idx * 32
            n = min 32 remaining
            w = v ! idx
         in [nucleotideAt w n i | i <- [0 .. n - 1]]
    revComp = map (`xor` 3) (reverse allNucs)
    packWords [] = []
    packWords ns =
        let (chunk, rest) = splitAt 32 ns
         in packWord chunk : packWords rest
    packWord = foldl (\acc nuc -> (acc `shiftL` 2) .|. nuc) 0