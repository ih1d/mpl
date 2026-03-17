module Primitives where

import Control.Monad.Except (throwError)
import Dataframe
import InterpM
import MPLTypes
import Syntax
import Prelude hiding (readFile)

applyPrint :: [Value] -> InterpM Value
applyPrint ((DataframeV df) : vals) = do
    io $ printDf df
    applyPrint vals
applyPrint vals = do
    io $ mapM_ print vals
    pure $ UnitV ()

applyRead :: FilePath -> InterpM Value
applyRead fp = do
    case fileType fp of
        Csv -> do
            mdf <- io $ readCsv fp
            case mdf of
                Nothing -> throwError $ RuntimeError "couldn't construct Dataframe."
                Just df -> pure $ DataframeV df
        _ -> throwError $ RuntimeError "not implemented for that file extension"
        
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

applyKmers :: [Value] -> InterpM Value
applyKmers ((DNAV dna) : (IntV k) : _) = do
    t <- io $ frequentKmers dna k
    pure $ DataframeV t
applyKmers ((DNAV _) : v : _) = throwError $ TypeError IntT (typeOf v)
applyKmers (v : _) = throwError $ TypeError DNAT (typeOf v)
applyKmers [] = throwError $ RuntimeError "kmers expects a DNA sequence and an integer"
