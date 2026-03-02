module Prims where

import Syntax
import MPL
import Prelude hiding (readFile)

applyPrint :: [Value] -> M Value
applyPrint vals = do
    io $ mapM_ print vals
    pure $ UnitV ()

applyReadCsv :: [Value] -> M Value
applyReadCsv = undefined