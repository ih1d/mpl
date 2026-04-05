module Primitives where

import Control.Monad.Except (throwError)
import InterpM
import MPLTypes
import Syntax

applyPrint :: [Value] -> InterpM Value
applyPrint vals = do
    io $ mapM_ print vals
    pure $ UnitV ()

applyRead :: FilePath -> InterpM Value
applyRead fp = do
    mdf <- io $ readFile fp
    pure $ StringV mdf

applyWrite :: FilePath -> Value -> InterpM Value
applyWrite fp val = do
    io $ writeFile fp (show val)
    pure $ UnitV ()

applyTranscribe :: [Value] -> InterpM Value
applyTranscribe ((DNAV dna) : _) = do
    let rna = transcribe dna
    pure $ RNAV rna
applyTranscribe (v : _) = throwError $ TypeError DNAT (typeOf v)
applyTranscribe [] = throwError $ RuntimeError "transcribe expects 1 argument"

applyCountNucleotides :: [Value] -> InterpM Value
applyCountNucleotides ((DNAV dna) : _) = do
    let (a, g, c, t) = countNucleotides dna
    pure $ TupleV [IntV a, IntV g, IntV c, IntV t]
applyCountNucleotides (v : _) = throwError $ TypeError DNAT (typeOf v)
applyCountNucleotides [] = throwError $ RuntimeError "count_nucleotides expects 1 argument"

applyReverseComplement :: [Value] -> InterpM Value
applyReverseComplement ((DNAV dna) : _) = do
    let dna' = reverseComplement dna
    pure $ DNAV dna'
applyReverseComplement (v : _) = throwError $ TypeError DNAT (typeOf v)
applyReverseComplement [] = throwError $ RuntimeError "reverse_complement expects 1 argument"
