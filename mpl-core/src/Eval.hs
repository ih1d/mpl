module Eval (runEval) where

import MPL
import Control.Monad.Except (throwError)
import Syntax
import Control.Monad (void)
import Parser

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
                (IntT, DoubleT) -> pure DoubleT
                (DoubleT, IntT) -> pure DoubleT
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        Sub -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, DoubleT) -> pure DoubleT
                (DoubleT, IntT) -> pure DoubleT
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        Mul -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, DoubleT) -> pure DoubleT
                (DoubleT, IntT) -> pure DoubleT
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
                (t, _) -> throwError $ TypeError NumT t
        Div -> 
            case (t0, t1) of
                (IntT, IntT) -> pure IntT
                (DoubleT, DoubleT) -> pure DoubleT
                (IntT, DoubleT) -> pure DoubleT
                (DoubleT, IntT) -> pure DoubleT
                (t, IntT) -> throwError $ TypeError NumT t
                (t, DoubleT) -> throwError $ TypeError NumT t
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
        And -> 
            case (v0, v1) of
                (BoolV b1, BoolV b2) -> pure  (BoolV (b1 && b2))
                _ -> throwError $ RuntimeError "expected booleans for &&"
        Or ->
            case (v0, v1) of
                (BoolV b1, BoolV b2) -> pure  (BoolV (b1 || b2))
                _ -> throwError $ RuntimeError "expected booleans for ||"
        Not -> throwError $ RuntimeError "not is a unary operator"
        Eq -> 
            case (v0, v1) of
                (IntV i1, IntV i2) -> pure (BoolV (i1 == i2))
                (DoubleV d1, DoubleV d2) -> pure (BoolV (d1 == d2))
                (DoubleV d, IntV i) -> pure (BoolV (d == fromInteger i))
                (IntV i, DoubleV d) -> pure (BoolV (fromInteger i == d))
                _ -> throwError $ RuntimeError "expectected numerical values for =="
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
                _ -> throwError $ RuntimeError "expectected numerical values for >="
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
        Pipe -> undefined
eval (If cnd e0 e1) = do
    cnd' <- eval cnd
    case cnd' of
        BoolV True -> eval e0
        BoolV False -> eval e1
        _ -> throwError $ RuntimeError "if expects bool"
eval (Var v) = lookupVar v >>= eval
eval (Let v e0 e1) = do
    bindVar v e0
    eval e1
eval (LetF{}) = undefined
eval (LetR{}) = undefined
eval (Lam _ _) = undefined
eval (App _ _) = undefined
eval (Primitive _) = undefined

runEval :: String -> IO (Either Error (Value, Types))
runEval str = case parser str of
    Left perr -> runM (throwError $ ParseE perr) initEnv
    Right expr -> do
        mt <- runM (tc expr) initEnv
        case mt of
            Left tr -> runM (throwError tr) initEnv
            Right t -> do
                mv <- runM (eval expr) initEnv
                case mv of
                    Left vr -> runM (throwError vr) initEnv
                    Right v -> pure $ Right (v, t)