module Primitives where

import Syntax
import Prelude hiding (readFile)
import InterpM
import Control.Monad.Except (throwError)
import MPLTypes (transcribe)

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

applyReadCsv :: [Value] -> InterpM Value
applyReadCsv = undefined