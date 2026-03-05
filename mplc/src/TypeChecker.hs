module TypeChecker where

import Control.Monad (unless, void)
import Control.Monad.Except (MonadError (throwError))
import InterpM
import Syntax

-- type checker
tc :: Expr -> InterpM Types
tc (Const (IntV _)) = pure IntT
tc (Const (DoubleV _)) = pure DoubleT
tc (Const (BoolV _)) = pure BoolT
tc (Const (StringV _)) = pure StringT
tc (Const (UnitV _)) = pure UnitT
tc (Const (ClosureV{})) = pure FunT
tc (Const (DNAV _)) = pure DNAT
tc (Const (RNAV _)) = pure RNAT
tc (Const (TupleV vs)) = pure $ TupleT (map typeOf vs)
tc (Const (ADTV t)) = lookupType t
tc (UnOp Not (Const (BoolV _))) = pure BoolT
tc (UnOp Not e) = do
    t <- tc e
    throwError $ TypeError BoolT t
tc (UnOp Sub (Const (IntV _))) = pure IntT
tc (UnOp Sub (Const (DoubleV _))) = pure DoubleT
tc (UnOp Sub e) = do
    t <- tc e
    throwError $ TypeError NumT t
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
                (IntT, IntT) -> pure DoubleT
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
tc (Let v e) =
    if contains v e
        then throwError $ NotInScope v e
        else do
            bindVar v e
            tc e
tc (LetI v e0 e1)
    | contains v e0 = throwError $ NotInScope v e0
    | contains v e1 = throwError $ NotInScope v e1
    | otherwise = do
        void $ tc e0
        bindVar v e0
        tc e1
tc (LetF f args body) =
    if contains f body
        then throwError $ NotInScope f body
        else do
            bindVar f (Lam args body)
            pure FunT
tc (LetR f args body) = do
    bindVar f (Lam args body)
    pure FunT
tc (Lam _ _) = pure FunT
tc (Tuple es) = TupleT <$> mapM tc es
tc (App f args) = do
    targs <- mapM tc args
    case f of
        Var "print" -> pure UnitT
        Var "read_csv" -> pure UnitT
        Var "transcribe" -> pure RNAT
        Var "count_nucleotides" -> pure $ TupleT [IntT, IntT, IntT, IntT]
        Var "reverse_complement" -> pure DNAT
        Lam vars body -> do
            mapM_ (uncurry bindVar) (zip vars args)
            tc body
        Var v -> do
            tf <- tc =<< lookupVar v
            unless (tf == FunT) (throwError $ TypeError FunT tf)
            pure $ last targs
        e -> tc e >>= throwError . TypeError FunT
tc (Type (t, _types)) = bindType t (ADTT t) >> pure (ADTT t)

contains :: Id -> Expr -> Bool
contains _ (Const _) = False
contains name (UnOp _ e) = contains name e
contains name (BinOp _ e0 e1) = contains name e0 || contains name e1
contains name (If cnd thn els) = contains name cnd || contains name thn || contains name els
contains name (Var n) = name == n
contains name (Let n e) = name == n || contains name e
contains name (LetI n e0 e1) = name == n || contains name e0 || contains name e1
contains name (LetF n args e) = name == n || elem name args || contains name e
contains _ (LetR{}) = False
contains name (Lam args e) = elem name args || contains name e
contains name (App f args) = contains name f || any (contains name) args
contains name (Tuple exprs) = any (contains name) exprs
contains _ (Type _) = False
