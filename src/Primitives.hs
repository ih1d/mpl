module Primitives where

import Control.Monad.Except (throwError)
import Dataframe
import InterpM
import MPLTypes
import Syntax
import Prelude hiding (readFile)

applyPrint :: [Value] -> InterpM Value
applyPrint ((DataframeV df) : vals) = do
    io $ printTable df
    applyPrint vals
applyPrint vals = do
    io $ mapM_ print vals
    pure $ UnitV ()

applyReadCsv :: [Value] -> InterpM Value
applyReadCsv ((StringV f) : _) = do
    t <- io $ readCsv f
    pure $ DataframeV t
applyReadCsv (v : _) = throwError $ TypeError StringT (typeOf v)
applyReadCsv [] = throwError $ RuntimeError "read_csv expects a filename"

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

applyFilter :: [Value] -> InterpM Value
applyFilter ((DataframeV t) : (StringV path) : (ClosureV _ body) : _) =
    case body of
        (BinOp Gt (Const (IntV i)) _) -> do
            t' <- io $ filterTableGt t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Gt (Const (DoubleV d)) _) -> do
            t' <- io $ filterTableGt t path d
            pure $ DataframeV t'
        (BinOp Gt _ (Const (IntV i))) -> do
            t' <- io $ filterTableGt t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Gt _ (Const (DoubleV d))) -> do
            t' <- io $ filterTableGt t path d
            pure $ DataframeV t'
        (BinOp Lt (Const (IntV i)) _) -> do
            t' <- io $ filterTableLt t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Lt (Const (DoubleV d)) _) -> do
            t' <- io $ filterTableLt t path d
            pure $ DataframeV t'
        (BinOp Lt _ (Const (IntV i))) -> do
            t' <- io $ filterTableLt t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Lt _ (Const (DoubleV d))) -> do
            t' <- io $ filterTableLt t path d
            pure $ DataframeV t'
        (BinOp Eq (Const (IntV i)) _) -> do
            t' <- io $ filterTableEq t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Eq (Const (DoubleV d)) _) -> do
            t' <- io $ filterTableEq t path d
            pure $ DataframeV t'
        (BinOp Eq _ (Const (IntV i))) -> do
            t' <- io $ filterTableEq t path (fromIntegral i)
            pure $ DataframeV t'
        (BinOp Eq _ (Const (DoubleV d))) -> do
            t' <- io $ filterTableEq t path d
            pure $ DataframeV t'
        _ -> throwError $ RuntimeError "filter expects: >, <, or =="
applyFilter ((DataframeV _) : (StringV _) : v) = throwError $ RuntimeError ("filter expects a function, got: " ++ show v)
applyFilter ((DataframeV _) : v : _) = throwError $ RuntimeError ("filter expects a column name (string), got: " ++ show v)
applyFilter (v : _) = throwError $ RuntimeError ("filter expects a dataframe, got: " ++ show v)
applyFilter [] = throwError $ RuntimeError "filter expects 3 arguments: a dataframe, a column name (string) and a function"

applyKmers :: [Value] -> InterpM Value
applyKmers ((DNAV dna) : (IntV k) : _) = do
    t <- io $ frequentKmers dna k
    pure $ DataframeV t
applyKmers ((DNAV _) : v : _) = throwError $ TypeError IntT (typeOf v)
applyKmers (v : _) = throwError $ TypeError DNAT (typeOf v)
applyKmers [] = throwError $ RuntimeError "kmers expects a DNA sequence and an integer"
