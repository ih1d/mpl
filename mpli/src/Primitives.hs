module Primitives where

import Syntax
import Prelude hiding (readFile)
import InterpM

applyPrint :: [Value] -> InterpM Value
applyPrint vals = do
    io $ mapM_ print vals
    pure $ UnitV ()

applyReadCsv :: [Value] -> InterpM Value
applyReadCsv = undefined