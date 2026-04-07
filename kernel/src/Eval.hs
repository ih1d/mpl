module Eval (initialEnv) where

import InterpM
import Syntax
import Dataframe

initEnv :: [(Id, Expr)]
initEnv =
    [ ("read10x", VarE "read10x")
    , ("read10x_h5", VarE "read10x_h5")
    , ("computeQCMetrics", VarE "computeQCMetrics")
    ]

initialEnv :: Env
initialEnv = Env initEnv 

-- evaluator
eval :: Stmt -> InterpM (Maybe Dataframe)
eval (Assign v e) = do
    bindVar v e
    pure Nothing
eval (ExprS e) = 
    case e of
        ReadE _r _str -> undefined
        VarE _v -> undefined