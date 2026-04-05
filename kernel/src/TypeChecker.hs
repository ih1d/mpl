module TypeChecker where

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
        Pipe ->
            case t0 of
                DataframeT -> pure t1
                t -> throwError $ TypeError DataframeT t
tc (Var v)
    | v `elem` builtins = pure FunT
    | otherwise = lookupVar v >>= tc
  where
    builtins =
        [ "print"
        , "read_csv"
        , "filter"
        , "transcribe"
        , "count_nucleotides"
        , "reverse_complement"
        , "kmers"
        ]
tc (Tuple es) = TupleT <$> mapM tc es
tc _ = undefined

contains :: Id -> Expr -> Bool
contains _ (Const _) = False
contains name (BinOp _ e0 e1) = contains name e0 || contains name e1
contains name (Var n) = name == n
contains name (Tuple exprs) = any (contains name) exprs
contains _ _ = False
