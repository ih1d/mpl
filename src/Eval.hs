module Eval (initialEnv, runEval, runEvalExpr) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import InterpM
import Parser
import Primitives
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
    , ("write", Var "write")
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
initialEnv = Env initEnv initTypeEnv

-- apply a function with scoped parameter bindings
applyFn :: [Id] -> Expr -> [Expr] -> InterpM Value
applyFn params body args = do
    env <- get
    vals <- mapM eval args
    mapM_ (\(p, v) -> bindVar p (Const v)) (zip params vals)
    result <- eval body
    put env
    pure result

-- evaluator
eval :: Expr -> InterpM Value
eval (Const i) = pure i
eval (UnOp Not (Const (BoolV b))) = pure $ BoolV (not b)
eval (UnOp Sub (Const (IntV i))) = pure $ IntV (-i)
eval (UnOp Sub (Const (DoubleV d))) = pure $ DoubleV (-d)
eval (UnOp op _) = throwError $ RuntimeError ("not unary operator: " ++ show op)
eval (BinOp Pipe _ _) = undefined
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
        And ->
            case (v0, v1) of
                (BoolV b1, BoolV b2) -> pure (BoolV (b1 && b2))
                _ -> throwError $ RuntimeError "expected booleans for &&"
        Or ->
            case (v0, v1) of
                (BoolV b1, BoolV b2) -> pure (BoolV (b1 || b2))
                _ -> throwError $ RuntimeError "expected booleans for ||"
        Not -> throwError $ RuntimeError "not is a unary operator"
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
eval (If cnd e0 e1) = do
    cnd' <- eval cnd
    case cnd' of
        BoolV True -> eval e0
        BoolV False -> eval e1
        _ -> throwError $ RuntimeError "if expects bool"
eval (Var v) = lookupVar v >>= eval
eval (LetI v e0 e1) = do
    bindVar v e0
    eval e1
eval (LetF f args body) = do
    bindVar f (Lam args body)
    pure $ ClosureV args body
eval (LetR f args body) = do
    bindVar f (Lam args body)
    pure $ ClosureV args body
eval (Lam args body) = pure $ ClosureV args body
eval (Tuple es) = TupleV <$> mapM eval es
eval (Read fp) = applyRead fp
eval (App f args) = do
    case f of
        Lam params body -> applyFn params body args
        Var v -> do
            expr <- lookupVar v
            case expr of
                Var "print" -> do
                    vals <- mapM eval args
                    applyPrint vals
                Var "transcribe" -> do
                    vals <- mapM eval args
                    applyTranscribe vals
                Var "count_nucleotides" -> do
                    vals <- mapM eval args
                    applyCountNucleotides vals
                Var "reverse_complement" -> do
                    vals <- mapM eval args
                    applyReverseComplement vals
                Var "kmers" -> do
                    vals <- mapM eval args
                    applyKmers vals
                Var "write" -> do
                    vals <- mapM eval args
                    applyWrite vals
                _ -> do
                    fVal <- eval expr
                    case fVal of
                        ClosureV params body -> applyFn params body args
                        _ -> throwError $ RuntimeError (v ++ " is not a function")
        _ -> do
            fVal <- eval f
            case fVal of
                ClosureV params body -> applyFn params body args
                _ -> throwError $ RuntimeError "application of non-function"
eval _ = undefined
