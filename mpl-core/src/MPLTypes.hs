module MPLTypes (
    DNA(..), 
    RNA(..),
    transcribe,
    reverseTranscribe,
) where

import Data.Vector (Vector, (!), toList, fromList)
import Data.Vector qualified as V
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
dnaChar 3 = 'T'
dnaChar n = error $ show n ++ " is not a DNA Value."

rnaChar :: Word64 -> Char
rnaChar 0 = 'A'
rnaChar 1 = 'C'
rnaChar 2 = 'G'
rnaChar 3 = 'U'
rnaChar n = error $ show n ++ " is not an RNA Value."

dnaWord :: Char -> Word64
dnaWord 'A' = 0
dnaWord 'C' = 1
dnaWord 'G' = 2
dnaWord 'T' = 3
dnaWord c = error $ c : " is not a DNA character."

rnaWord :: Char -> Word64
rnaWord 'A' = 0
rnaWord 'C' = 1
rnaWord 'G' = 2
rnaWord 'U' = 3
rnaWord c = error $ c : " is not a DNA character."

dnaToString :: DNA -> String
dnaToString (DNA (vec,_)) = toList $ V.map dnaChar vec

rnaToString :: RNA -> String
rnaToString (RNA (vec,_)) = toList $ V.map rnaChar vec

transcribe :: DNA -> RNA
transcribe dna@(DNA (_, l)) = 
    let dnaString = dnaToString dna
    in RNA (fromList $ map (rnaWord . transcribe') dnaString, l)
    where
        transcribe' :: Char -> Char
        transcribe' 'T' = 'U'
        transcribe' c = c

reverseTranscribe :: RNA -> DNA
reverseTranscribe rna@(RNA (_, l)) = 
    let rnaString = rnaToString rna
    in DNA (fromList $ map (dnaWord . revTranscribe) rnaString, l)
    where
        revTranscribe :: Char -> Char
        revTranscribe 'U' = 'T'
        revTranscribe c = c