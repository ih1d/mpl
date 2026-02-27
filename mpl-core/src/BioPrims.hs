module BioPrims (DNA(..), RNA(..)) where

import Data.Vector (Vector, (!))
import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)

newtype DNA = DNA (Vector Word64, Int)
instance Show DNA where
    show (DNA (ws, len)) = showSeq dnaChar ws len

newtype RNA = RNA (Vector Word64, Int)
instance Show RNA where
    show (RNA (ws, len)) = showSeq rnaChar ws len

showSeq :: (Word64 -> Char) -> Vector Word64 -> Int -> String
showSeq decode v = go 0
  where
    go _ 0 = []
    go idx remaining =
        let w = v ! idx
            n = min 32 remaining
        in [decode ((w `shiftR` (2 * (n - 1 - i))) .&. 3) | i <- [0..n-1]] ++ go (idx + 1) (remaining - n)

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
