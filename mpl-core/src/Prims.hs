module Prims where

import Syntax
import MPL
import Prelude hiding (readFile)

applyPrint :: Expr -> M Value
applyPrint e = do
    io $ print e
    pure $ UnitV ()
