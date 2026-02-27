module Eval (eval, initEnv, runM, tc) where

import MPL
import Control.Monad.Except (throwError)
import Syntax
import Control.Monad (void)

initEnv :: Env
initEnv = 
    [ ("print", Primitive "print")
    , ("complement", Primitive "complement")
    , ("transcribe", Primitive "transcribe")
    , ("translate", Primitive "translate")
    , ("reverse_complement", Primitive "reverse_complement")
    , ("read_csv", Primitive "read_csv")
    , ("read_fastq", Primitive "read_fastq")
    ]

-- type checker
tc :: Expr -> M Types
tc (Const (IntV _)) = pure IntT
tc (Const (DoubleV _)) = pure DoubleT
tc (Const (BoolV _)) = pure BoolT
tc (Const (StringV _)) = pure StringT
tc (Const (UnitV _)) = pure UnitT
tc (Const (ClosureV {})) = pure FunT
tc (Const (DNAV _)) = pure DNAT
tc (Const (RNAV _)) = pure RNAT
tc (UnOp Not (Const (BoolV _))) = pure BoolT
tc (UnOp Not e) = do
    t <- tc e
    throwError $ TypeError BoolT t
tc (UnOp op _) = throwError $ RuntimeError ("operator: " ++ show op ++ " is not unary")
tc (BinOp op e0 e1) = do
    (t0, t1) <- (,) <$> tc e0 <*> tc e1
    case op of
        Add -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, t) -> throwError $ TypeError IntT t
                (DoubleT, t) -> throwError $ TypeError DoubleT t
                (t, IntT) -> throwError $ TypeError IntT t
                (t, DoubleT) -> throwError $ TypeError DoubleT t
                (t, _) -> throwError $ TypeError NumT t
        Sub -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, t) -> throwError $ TypeError IntT t
                (DoubleT, t) -> throwError $ TypeError DoubleT t
                (t, IntT) -> throwError $ TypeError IntT t
                (t, DoubleT) -> throwError $ TypeError DoubleT t
                (t, _) -> throwError $ TypeError NumT t
        Mul -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, t) -> throwError $ TypeError IntT t
                (DoubleT, t) -> throwError $ TypeError DoubleT t
                (t, IntT) -> throwError $ TypeError IntT t
                (t, DoubleT) -> throwError $ TypeError DoubleT t
                (t, _) -> throwError $ TypeError NumT t
        Div -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, t) -> throwError $ TypeError IntT t
                (DoubleT, t) -> throwError $ TypeError DoubleT t
                (t, IntT) -> throwError $ TypeError IntT t
                (t, DoubleT) -> throwError $ TypeError DoubleT t
                (t, _) -> throwError $ TypeError NumT t
        Pow -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, DoubleT) -> pure DoubleT
                (DoubleT, IntT) -> pure DoubleT
                (IntT, t) -> throwError $ TypeError NumT t
                (DoubleT, t) -> throwError $ TypeError NumT t
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        And -> 
            case (t0, t1) of
                (BoolT, BoolT) -> pure BoolT
                (BoolT, t) -> throwError $ TypeError BoolT t
                (t, _) -> throwError $ TypeError BoolT t
        Or -> 
            case (t0, t1) of
                (BoolT, BoolT) -> pure BoolT
                (BoolT, t) -> throwError $ TypeError BoolT t
                (t, _) -> throwError $ TypeError BoolT t
        Eq -> if t0 == t1 then pure BoolT else throwError $ TypeError t0 t1
        NotEq -> if t0 == t1 then pure BoolT else throwError $ TypeError t0 t1
        Gt -> 
            case (t0, t1) of
                (IntT, IntT) -> pure BoolT
                (DoubleT, DoubleT) -> pure BoolT
                (IntT, DoubleT) -> pure BoolT
                (DoubleT, IntT) -> pure BoolT
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (IntT, t) -> throwError $ TypeError NumT t
                (DoubleT, t) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        GtEq -> 
            case (t0, t1) of
                (IntT, IntT) -> pure BoolT
                (DoubleT, DoubleT) -> pure BoolT
                (IntT, DoubleT) -> pure BoolT
                (DoubleT, IntT) -> pure BoolT
                (IntT, t) -> throwError $ TypeError NumT t
                (DoubleT, t) -> throwError $ TypeError NumT t
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        Lt -> 
            case (t0, t1) of
                (IntT, IntT) -> pure BoolT
                (DoubleT, DoubleT) -> pure BoolT
                (IntT, DoubleT) -> pure BoolT
                (DoubleT, IntT) -> pure BoolT
                (IntT, t) -> throwError $ TypeError NumT t
                (DoubleT, t) -> throwError $ TypeError NumT t
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        LtEq -> 
            case (t0, t1) of
                (IntT, IntT) -> pure BoolT
                (DoubleT, DoubleT) -> pure BoolT
                (IntT, DoubleT) -> pure BoolT
                (DoubleT, IntT) -> pure BoolT
                (IntT, t) -> throwError $ TypeError NumT t
                (DoubleT, t) -> throwError $ TypeError NumT t
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        Pipe -> undefined
        Not -> throwError $ RuntimeError "not is an unary operator"
tc (If cnd e0 e1) = do
    tcnd <- tc cnd
    case tcnd of
        BoolT -> do
            te0 <- tc e0
            te1 <- tc e1
            if te0 == te1 then pure te0 else throwError $ TypeError te0 te1
        _ -> throwError $ TypeError BoolT tcnd
tc (Var v) = lookupVar v >>= tc
tc (Let _ e0 e1) = do
    void $ tc e0
    tc e1
tc (LetF _ _ e) = tc e
tc (LetR _ _ e) = tc e
tc (Lam _ _) = pure FunT
tc (App f _) = tc f
tc (Primitive _) = pure PrimitiveT


-- evaluator
eval :: Expr -> M Value
eval (Const i) = pure i
eval (UnOp Not (Const (BoolV b))) = pure $ BoolV (not b)
eval (UnOp op _) = throwError $ RuntimeError ("not unary operator: " ++ show op)
eval (BinOp op e0 e1) =do
    (v0, v1) <- (,) <$> eval e0 <*> eval e1
    case op of
        Add -> 
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 + i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 + d2))
                _ -> undefined
        Sub -> 
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 - i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 - d2))
                _ -> undefined
        Mul -> 
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 * i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 * d2))
                _ -> undefined
        Div ->
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 `div` i2))
                (DoubleV d1, DoubleV d2) -> pure (DoubleV (d1 / d2))
                _ -> undefined
        Pow -> 
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (IntV (i1 ^ i2))
                _ -> undefined
        And -> undefined
        Or -> undefined
        Not -> undefined
        Eq -> undefined
        NotEq -> undefined
        Gt -> undefined
        GtEq -> undefined
        Lt -> undefined
        LtEq -> undefined
        Pipe -> undefined
eval (If cnd e0 e1) = do
    cnd' <- eval cnd
    case cnd' of
        BoolV True -> eval e0
        BoolV False -> eval e1
        _ -> do
            throwError $ RuntimeError "if expects bool"
eval (Var v) = lookupVar v >>= eval
eval (Let v e0 e1) = do
    bindVar v e0
    eval e1
eval (LetF{}) = undefined
eval (LetR{}) = undefined
eval (Lam _ _) = undefined
eval (App _ _) = undefined
eval (Primitive _) = undefined