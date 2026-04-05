module Eval (initialEnv, runEval, runEvalExpr) where

import Control.Monad.Except (MonadError (throwError))
import InterpM
import Parser
import Syntax
import TypeChecker

runEval :: Env -> String -> IO (Either Error (Value, Types), Env)
runEval env str = case parser str of
    Left perr -> do
        r <- runM (throwError $ ParseE perr) env
        pure (r, env)
    Right expr -> runEvalExpr env expr

runEvalExpr :: Env -> Expr -> IO (Either Error (Value, Types), Env)
runEvalExpr env expr = do
    (mt, _) <- runMState (tc expr) env
    case mt of
        Left tr -> do
            r <- runM (throwError tr) env
            pure (r, env)
        Right t -> do
            (mv, env') <- runMState (eval expr) env
            case mv of
                Left vr -> do
                    r <- runM (throwError vr) env
                    pure (r, env)
                Right v -> pure (Right (v, t), env')

initEnv :: [(Id, Expr)]
initEnv =
    [ ("print", Var "print")
    , ("count_nucleotides", Var "count_nucleotides")
    , ("transcribe", Var "transcribe")
    , ("reverse_complement", Var "reverse_complement")
    , ("kmers", Var "kmers")
    ]

initTypeEnv :: [(Id, Types)]
initTypeEnv =
    [ (show IntT, IntT)
    , (show BoolT, BoolT)
    , (show StringT, StringT)
    , (show NumT, NumT)
    , (show DoubleT, DoubleT)
    , (show FunT, FunT)
    , (show DNAT, DNAT)
    , (show RNAT, RNAT)
    , (show UnitT, UnitT)
    ]

initialEnv :: Env
initialEnv = Env initEnv initTypeEnv 1

-- evaluator
eval :: Expr -> InterpM Value
eval (Const i) = pure i
eval (BinOp op e0 e1) = do
    (v0, v1) <- (,) <$> eval e0 <*> eval e1
    case op of
        Add ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 + i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 + d2))
                (DoubleV d, IntV i) -> pure (DoubleV (d + fromInteger i))
                (IntV i, DoubleV d) -> pure (DoubleV (d + fromInteger i))
                _ -> throwError $ RuntimeError "expectected numerical values for +"
        Sub ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 - i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 - d2))
                (DoubleV d, IntV i) -> pure (DoubleV (d - fromInteger i))
                (IntV i, DoubleV d) -> pure (DoubleV (fromInteger i - d))
                _ -> throwError $ RuntimeError "expectected numerical values for -"
        Mul ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 * i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 * d2))
                (DoubleV d, IntV i) -> pure (DoubleV (d * fromInteger i))
                (IntV i, DoubleV d) -> pure (DoubleV (d * fromInteger i))
                _ -> throwError $ RuntimeError "expectected numerical values for *"
        Div ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (DoubleV (fromInteger i1 / fromInteger i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 / d2))
                (DoubleV d, IntV i) -> pure (DoubleV (d / fromInteger i))
                (IntV i, DoubleV d) -> pure (DoubleV (fromInteger i / d))
                _ -> throwError $ RuntimeError "expectected numerical values for /"
        Pow ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 ^ i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 ** d2))
                (DoubleV d, IntV i) -> pure (DoubleV (d ** fromInteger i))
                (IntV i, DoubleV d) -> pure (DoubleV (fromInteger i ** d))
                _ -> throwError $ RuntimeError "expectected numerical values for ^"
        Eq -> if v0 == v1 then pure (BoolV True) else pure (BoolV False)
        NotEq ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 /= i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 /= d2))
                (DoubleV d, IntV i) -> pure (BoolV (d /= fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i /= d))
                _ -> throwError $ RuntimeError "expectected numerical values for !="
        Gt ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 > i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 > d2))
                (DoubleV d, IntV i) -> pure (BoolV (d > fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i > d))
                _ -> throwError $ RuntimeError "expectected numerical values for >"
        GtEq ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 >= i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 >= d2))
                (DoubleV d, IntV i) -> pure (BoolV (d >= fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i >= d))
                _ -> throwError $ RuntimeError "expectected numerical values for >="
        Lt ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 < i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 < d2))
                (DoubleV d, IntV i) -> pure (BoolV (d < fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i < d))
                _ -> throwError $ RuntimeError "expectected numerical values for <"
        LtEq ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 <= i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 <= d2))
                (DoubleV d, IntV i) -> pure (BoolV (d <= fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i <= d))
                _ -> throwError $ RuntimeError "expectected numerical values for <="
        _ -> undefined
eval (Var v) = lookupVar v >>= eval
eval (Tuple es) = TupleV <$> mapM eval es
eval _ = undefined
