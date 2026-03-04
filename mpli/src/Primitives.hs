module Primitives where

import Control.Monad.Except (throwError)
import InterpM
import MPLTypes (countNucleotides, transcribe)
import Syntax
import Prelude hiding (readFile)

applyPrint :: [Value] -> InterpM Value
applyPrint vals = do
    io $ mapM_ print vals
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
    pure $ Tuple [IntV a, IntV g, IntV c, IntV t]
applyCountNucleotides (v : _) = throwError $ TypeError DNAT (typeOf v)
applyCountNucleotides [] = throwError $ RuntimeError "count_nucleotides expects 1 argument"

applyReadCsv :: [Value] -> InterpM Value
applyReadCsv = undefined

applyReadTsv :: [Value] -> InterpM Value
applyReadTsv = undefined
